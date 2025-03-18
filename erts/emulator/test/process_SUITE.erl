%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

-module(process_SUITE).

%% Tests processes, trapping exit messages and the BIFs:
%% 	exit/1
%%	exit/2
%%	process_info/1,2
%%      suspend_process/2 (partially)
%%	register/2 (partially)

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-define(heap_binary_size, 64).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, spawn_with_binaries/1,
	 t_exit_1/1, t_exit_2_other/1, t_exit_2_other_normal/1,
	 self_exit/1, normal_suicide_exit/1, abnormal_suicide_exit/1,
	 t_exit_2_catch/1, trap_exit_badarg/1, trap_exit_badarg_in_bif/1,
	 exit_and_timeout/1, exit_twice/1,
	 t_process_info/1, process_info_other/1, process_info_other_msg/1,
         process_info_other_message_queue_len_signal_race/1,
	 process_info_other_dist_msg/1,
         process_info_other_status/1,
	 process_info_2_list/1, process_info_lock_reschedule/1,
	 process_info_lock_reschedule2/1,
	 process_info_lock_reschedule3/1,
         process_info_garbage_collection/1,
         process_info_parent/1,
         process_info_smoke_all/1,
         process_info_status_handled_signal/1,
         process_info_reductions/1,
         process_info_self_signal/1,
         process_info_self_msgq_len/1,
         process_info_self_msgq_len_messages/1,
         process_info_self_msgq_len_more/1,
         process_info_msgq_len_no_very_long_delay/1,
         process_info_dict_lookup/1,
         process_info_label/1,
         suspend_process_pausing_proc_timer/1,
	 bump_reductions/1, low_prio/1, binary_owner/1, yield/1, yield2/1,
	 otp_4725/1, dist_unlink_ack_exit_leak/1, bad_register/1,
         garbage_collect/1, otp_6237/1,
	 process_info_messages/1, process_flag_badarg/1,
         process_flag_fullsweep_after/1, process_flag_heap_size/1,
         command_line_max_heap_size/1,
	 spawn_opt_heap_size/1, spawn_opt_max_heap_size/1,
	 processes_large_tab/1, processes_default_tab/1, processes_small_tab/1,
	 processes_this_tab/1, processes_apply_trap/1,
	 processes_last_call_trap/1, processes_gc_trap/1,
	 processes_term_proc_list/1,
         processes_send_infant/1,
	 otp_7738_waiting/1, otp_7738_suspended/1,
	 otp_7738_resume/1,
	 garb_other_running/1,
	 no_priority_inversion/1,
	 no_priority_inversion2/1,
	 system_task_blast/1,
	 system_task_on_suspended/1,
         system_task_failed_enqueue/1,
	 gc_request_when_gc_disabled/1,
	 gc_request_blast_when_gc_disabled/1,
         otp_16436/1,
         otp_16642/1,
         spawn_huge_arglist/1,
         spawn_request_bif/1,
         spawn_request_monitor_demonitor/1,
         spawn_request_monitor_child_exit/1,
         spawn_request_link_child_exit/1,
         spawn_request_link_parent_exit/1,
         spawn_request_link_parent_exit_compound_reason/1,
         spawn_request_link_parent_exit_nodedown/1,
         spawn_request_abandon_bif/1,
         dist_spawn_monitor/1,
         spawn_against_ei_node/1,
         spawn_against_old_node/1,
         spawn_against_new_node/1,
         spawn_request_reply_option/1,
         dist_spawn_arg_list_mixup/1,
         alias_bif/1,
         dist_frag_alias/1,
         dist_frag_unaliased/1,
         monitor_alias/1,
         spawn_monitor_alias/1,
         demonitor_aliasmonitor/1,
         down_aliasmonitor/1,
         monitor_tag/1,
         no_pid_wrap/1,
         processes_iter/1]).

-export([prio_server/2, prio_client/2, init/1, handle_event/2]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([hangaround/2, processes_bif_test/0, do_processes/1,
	 processes_term_proc_list_test/1, huge_arglist_child/255]).

-export([spawn_request_test_exit_child/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 9}}].

all() ->
    [spawn_with_binaries, t_exit_1, {group, t_exit_2},
     trap_exit_badarg, trap_exit_badarg_in_bif,
     bump_reductions, low_prio, yield, yield2, otp_4725,
     dist_unlink_ack_exit_leak, bad_register, garbage_collect,
     process_flag_badarg,
     process_flag_fullsweep_after, process_flag_heap_size,
     command_line_max_heap_size,
     spawn_opt_heap_size, spawn_opt_max_heap_size,
     spawn_huge_arglist,
     otp_6237,
     {group, spawn_request},
     {group, process_info_bif},
     {group, suspend_process_bif},
     {group, processes_bif},
     {group, otp_7738}, garb_other_running,
     {group, system_task},
     {group, alias},
     monitor_tag,
     no_pid_wrap].

groups() -> 
    [{t_exit_2, [],
      [t_exit_2_other, t_exit_2_other_normal, self_exit,
       normal_suicide_exit, abnormal_suicide_exit,
       t_exit_2_catch, exit_and_timeout, exit_twice]},
     {spawn_request, [],
      [spawn_request_bif,
       spawn_request_monitor_demonitor,
       spawn_request_monitor_child_exit,
       spawn_request_link_child_exit,
       spawn_request_link_parent_exit,
       spawn_request_link_parent_exit_compound_reason,
       spawn_request_link_parent_exit_nodedown,
       spawn_request_abandon_bif,
       dist_spawn_monitor,
       spawn_against_ei_node,
       spawn_against_old_node,
       spawn_against_new_node,
       spawn_request_reply_option,
       dist_spawn_arg_list_mixup]},
     {processes_bif, [],
      [processes_large_tab, processes_default_tab,
       processes_small_tab, processes_this_tab,
       processes_last_call_trap, processes_apply_trap,
       processes_gc_trap, processes_term_proc_list,
       processes_send_infant,
       processes_iter]},
     {process_info_bif, [],
      [t_process_info, process_info_messages,
       process_info_other, process_info_other_msg,
       process_info_other_message_queue_len_signal_race,
       process_info_other_dist_msg, process_info_other_status,
       process_info_2_list,
       process_info_lock_reschedule,
       process_info_lock_reschedule2,
       process_info_lock_reschedule3,
       process_info_garbage_collection,
       process_info_parent,
       process_info_smoke_all,
       process_info_status_handled_signal,
       process_info_reductions,
       process_info_self_signal,
       process_info_self_msgq_len,
       process_info_self_msgq_len_messages,
       process_info_self_msgq_len_more,
       process_info_msgq_len_no_very_long_delay,
       process_info_dict_lookup,
       process_info_label]},
     {suspend_process_bif, [],
      [suspend_process_pausing_proc_timer]},
     {otp_7738, [],
      [otp_7738_waiting, otp_7738_suspended,
       otp_7738_resume]},
     {system_task, [],
      [no_priority_inversion, no_priority_inversion2,
       system_task_blast, system_task_on_suspended, system_task_failed_enqueue,
       gc_request_when_gc_disabled, gc_request_blast_when_gc_disabled,
       otp_16436, otp_16642]},
     {alias, [],
      [alias_bif, monitor_alias, spawn_monitor_alias,
       demonitor_aliasmonitor, down_aliasmonitor,
       dist_frag_alias, dist_frag_unaliased]}].

init_per_suite(Config) ->
    A0 = case application:start(sasl) of
	     ok -> [sasl];
	     _ -> []
	 end,
    A = case application:start(os_mon) of
	     ok -> [os_mon|A0];
	     _ -> A0
	 end,
    [{started_apps, A}|Config].

end_per_suite(Config) ->
    As = proplists:get_value(started_apps, Config),
    lists:foreach(fun (A) -> application:stop(A) end, As),
    catch erts_debug:set_internal_state(available_internal_state, false),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Func, Config)
  when Func =:= processes_default_tab;
       Func =:= processes_this_tab ->
    case erlang:system_info(build_type) of
        BT when BT =:= debug; BT =:= valgrind ->
            {skip, "Don't run in debug/valgrind"};
        _ ->
            [{testcase, Func} | Config]
    end;
init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    %% Restore max_heap_size to default value.
    erlang:system_flag(max_heap_size,
                       #{size => 0,
                         kill => true,
                         include_shared_binaries => false,
                         error_logger => true}),
    erts_test_utils:ept_check_leaked_nodes(Config).

fun_spawn(Fun) ->
    spawn_link(erlang, apply, [Fun, []]).

%% Tests that binaries as arguments to spawn/3 doesn't leak
%% (unclear if this test case will actually prove anything on
%% a modern computer with lots of memory).
spawn_with_binaries(Config) when is_list(Config) ->
    L = lists:duplicate(2048, 42),
    TwoMeg = lists:duplicate(1024, L),
    Fun = fun() -> spawn(?MODULE, binary_owner, [list_to_binary(TwoMeg)]),
			 receive after 1 -> ok end end,
    Iter = case test_server:is_valgrind() of
		     true -> 10;
		     false -> 150
		 end,
    test_server:do_times(Iter, Fun),
    ok.

binary_owner(Bin) when is_binary(Bin) ->
    ok.

%% Tests exit/1 with a big message.
t_exit_1(Config) when is_list(Config) ->
    ct:timetrap({seconds, 20}),
    start_spawner(),
    process_flag(trap_exit, true),
    test_server:do_times(10, fun t_exit_1/0),
    stop_spawner(),
    ok.

t_exit_1() ->
    Pid = fun_spawn(fun() -> exit(kb_128()) end),
    Garbage = kb_128(),
    receive
	      {'EXIT', Pid, Garbage} -> ok
	  end.


%% Tests exit/2 with a lot of data in the exit message.
t_exit_2_other(Config) when is_list(Config) ->
    ct:timetrap({seconds, 20}),
    start_spawner(),
    process_flag(trap_exit, true),
    test_server:do_times(10, fun t_exit_2_other/0),
    stop_spawner(),
    ok.

t_exit_2_other() ->
    Pid = fun_spawn(fun() -> receive x -> ok end end),
    Garbage = kb_128(),
    exit(Pid, Garbage),
    receive
	      {'EXIT', Pid, Garbage} -> ok
	  end.

%% Tests that exit(Pid, normal) does not kill another process.;
t_exit_2_other_normal(Config) when is_list(Config) ->
    ct:timetrap({seconds, 20}),
    process_flag(trap_exit, true),
    Pid = fun_spawn(fun() -> receive x -> ok end end),
    exit(Pid, normal),
    receive
	      {'EXIT', Pid, Reason} ->
		  ct:fail({process_died, Reason})
	  after 1000 ->
		  ok
	  end,
    case process_info(Pid) of
	      undefined ->
		  ct:fail(process_died_on_normal);
	      List when is_list(List) ->
		  ok
	  end,
    exit(Pid, kill),
    ok.

%% Tests that we can trap an exit message sent with exit/2 from
%% the same process.
self_exit(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),
    start_spawner(),
    process_flag(trap_exit, true),
    test_server:do_times(200, fun self_exit/0),
    stop_spawner(),
    ok.

self_exit() ->
    Garbage = eight_kb(),
    P = self(),
    true = exit(P, Garbage),
    receive
	      {'EXIT', P, Garbage} -> ok
	  end.

%% Tests exit(self(), normal) is equivalent to exit(normal) for a process
%% that doesn't trap exits.
normal_suicide_exit(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pid = fun_spawn(fun() -> exit(self(), normal) end),
    receive
	      {'EXIT', Pid, normal} -> ok;
	      Other -> ct:fail({bad_message, Other})
	  end.

%% Tests exit(self(), Term) is equivalent to exit(Term) for a process
%% that doesn't trap exits.";
abnormal_suicide_exit(Config) when is_list(Config) ->
    Garbage = eight_kb(),
    process_flag(trap_exit, true),
    Pid = fun_spawn(fun() -> exit(self(), Garbage) end),
    receive
	      {'EXIT', Pid, Garbage} -> ok;
	      Other -> ct:fail({bad_message, Other})
	  end.

%% Tests that exit(self(), die) cannot be caught.
t_exit_2_catch(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pid = fun_spawn(fun() -> catch exit(self(), die) end),
    receive
	      {'EXIT', Pid, normal} ->
		  ct:fail(catch_worked);
	      {'EXIT', Pid, die} ->
		  ok;
	      Other ->
		  ct:fail({bad_message, Other})
	  end.

%% Tests trapping of an 'EXIT' message generated by a bad argument to
%% the abs/1 bif.  The 'EXIT' message will intentionally be very big.
trap_exit_badarg(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),
    start_spawner(),
    process_flag(trap_exit, true),
    try
        %% suppress  =ERROR REPORT=== emulator messages
        ok = logger:add_primary_filter(suppress_log_spam, {
            fun(#{meta := #{error_logger := #{emulator := true, tag := error}}}, _Report) ->
                stop;
            (_Meta, _Report) ->
                ignore
            end, ok}),
        test_server:do_times(10, fun trap_exit_badarg/0),
        ct:sleep(500) %% flush logging
    after
        ok = logger:remove_primary_filter(suppress_log_spam)
    end,
    stop_spawner(),
    ok.

trap_exit_badarg() ->
    Pid = fun_spawn(fun() -> bad_guy(kb_128()) end),
    Garbage = kb_128(),
    receive
	      {'EXIT',Pid,{badarg,[{erlang,abs,[Garbage],Loc1},
				   {?MODULE,bad_guy,1,Loc2}|_]}}
	      when is_list(Loc1), is_list(Loc2) ->
		  ok;
	      Other ->
		  ok = io:format("Bad EXIT message: ~P", [Other, 30]),
		  ct:fail(bad_exit_message)
	  end.

bad_guy(Arg) ->
    abs(Arg).


kb_128() ->
    Eight = eight_kb(),
    {big_binary(),
     Eight, Eight, Eight, Eight, Eight, Eight, Eight, Eight,
     big_binary(),
     Eight, Eight, Eight, Eight, Eight, Eight, Eight, Eight,
     big_binary()}.

eight_kb() ->
    B64 = lists:seq(1, 64),
    B512 = {<<1>>,B64,<<2,3>>,B64,make_unaligned_sub_binary(<<4,5,6,7,8,9>>),
		  B64,make_sub_binary([1,2,3,4,5,6]),
		  B64,make_sub_binary(lists:seq(1, ?heap_binary_size+1)),
		  B64,B64,B64,B64,big_binary()},
    lists:duplicate(8, {B512,B512}).

big_binary() ->
    big_binary(10, [42]).
big_binary(0, Acc) ->
    list_to_binary(Acc);
big_binary(N, Acc) ->
    big_binary(N-1, [Acc|Acc]).

%% Test receiving an EXIT message when spawning a BIF with bad arguments.
trap_exit_badarg_in_bif(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),
    process_flag(trap_exit, true),
    test_server:do_times(10, fun trap_exit_badarg_bif/0),
    ok.
    
trap_exit_badarg_bif() ->
    Pid = spawn_link(erlang, node, [1]),
    receive
	      {'EXIT', Pid, {badarg, _}} ->
		  ok;
	      Other ->
		  ct:fail({unexpected, Other})
	  end.

%% The following sequences of events have crasched Beam.
%%
%% 1) An exit is sent to a process which is currently not running.
%%    The exit reason will (on purpose) overwrite the message queue
%%    pointer.
%% 2) Before the process is scheduled in, it receives a timeout (from
%%    a 'receive after').
%% 3) The process will crash the next time it executes 'receive'.

exit_and_timeout(Config) when is_list(Config) ->
    ct:timetrap({seconds, 20}),

    process_flag(trap_exit, true),
    Parent = self(),
    Low = fun_spawn(fun() -> eat_low(Parent) end),
    High = fun_spawn(fun() -> eat_high(Low) end),
    eat_wait_for(Low, High),
    ok.


eat_wait_for(Low, High) ->
    receive
	{'EXIT', Low, {you, are, dead}} ->
	    ok;
	{'EXIT', High, normal} ->
	    eat_wait_for(Low, High);
	Other ->
	    ct:fail({bad_message, Other})
    end.

eat_low(_Parent) ->
    receive
    after 2500 ->
	    ok
    end,
    receive
	Any ->
	    io:format("Received: ~p\n", [Any])
    after 1000 ->
	    ok
    end.
    
eat_high(Low) ->
    process_flag(priority, high),
    receive after 1000 -> ok end,
    exit(Low, {you, are, dead}),
    loop(erlang:monotonic_time() + erlang:convert_time_unit(5,second,native)).

%% Busy loop for 5 seconds.

loop(StopTime) ->
    case StopTime >= erlang:monotonic_time() of
	true -> ok;
	false -> loop(StopTime)
    end.


%% Tries to send two different exit messages to a process.
%% (The second one should be ignored.)
exit_twice(Config) when is_list(Config) ->
    ct:timetrap({seconds, 20}),

    process_flag(trap_exit, true),
    Low = fun_spawn(fun etwice_low/0),
    High = fun_spawn(fun() -> etwice_high(Low) end),
    etwice_wait_for(Low, High),
    ok.

etwice_wait_for(Low, High) ->
    receive
	{'EXIT', Low, first} ->
	    ok;
	{'EXIT', Low, Other} ->
	    ct:fail({wrong_exit_reason, Other});
	{'EXIT', High, normal} ->
	    etwice_wait_for(Low, High);
	Other ->
	    ct:fail({bad_message, Other})
    end.

etwice_low() ->
    etwice_low().

etwice_high(Low) ->
    process_flag(priority, high),
    exit(Low, first),
    exit(Low, second).

%% Tests the process_info/2 BIF.
t_process_info(Config) when is_list(Config) ->
    [] = process_info(self(), registered_name),
    register(my_name, self()),
    {registered_name, my_name} = process_info(self(), registered_name),
    {status, running} = process_info(self(), status),
    {min_heap_size, 233} = process_info(self(), min_heap_size),
    {min_bin_vheap_size,46422} = process_info(self(), min_bin_vheap_size),
    {max_heap_size, #{ size := 0, kill := true, error_logger := true,
                       include_shared_binaries := false}} =
        process_info(self(), max_heap_size),
    {current_function,{?MODULE,t_process_info,1}} =
	process_info(self(), current_function),
    {current_function,{?MODULE,t_process_info,1}} =
	apply(erlang, process_info, [self(),current_function]),

    %% current_location and current_stacktrace
    {Line1,Res1} = {?LINE,process_info(self(), current_location)},
    verify_loc(Line1, Res1),
    {Line2,Res2} = {?LINE,apply(erlang, process_info,
				[self(),current_location])},
    verify_loc(Line2, Res2),
    pi_stacktrace([{?MODULE,t_process_info,1,?LINE}]),

    verify_stacktrace_depth(),

    Gleader = group_leader(),
    {group_leader, Gleader} = process_info(self(), group_leader),
    {'EXIT',{badarg,_Info}} = (catch process_info('not_a_pid')),
    ok.

verify_stacktrace_depth() ->
    CS = current_stacktrace,
    OldDepth = erlang:system_flag(backtrace_depth, 0),
    {CS,[]} = erlang:process_info(self(), CS),
    _ = erlang:system_flag(backtrace_depth, 8),
    {CS,[{?MODULE,verify_stacktrace_depth,0,_},_|_]} =
        erlang:process_info(self(), CS),
    _ = erlang:system_flag(backtrace_depth, OldDepth).

pi_stacktrace(Expected0) ->
    {Line,Res} = {?LINE,erlang:process_info(self(), current_stacktrace)},
    {current_stacktrace,Stack} = Res,
    Expected = [{?MODULE,pi_stacktrace,1,Line}|Expected0],
    pi_stacktrace_1(Stack, Expected).

pi_stacktrace_1([{M,F,A,Loc}|Stk], [{M,F,A,Line}|Exp]) ->
    case Loc of
	[] ->
	    %% No location info for some reason (+L, native code).
	    io:format("Missing location information for ~w:~w/~w",
		      [M,F,A]),
	    ok;
	[_|_] ->
	    Line = proplists:get_value(line, Loc),
	    File = proplists:get_value(file, Loc),
	    File = ?MODULE_STRING ++ ".erl"
    end,
    pi_stacktrace_1(Stk, Exp);
pi_stacktrace_1([_|_], []) -> ok.

verify_loc(Line, {current_location,{?MODULE,t_process_info=F,1=A,Loc}}) ->
    case Loc of
	[] ->
	    %% No location info for some reason (+L, native code).
	    io:format("Missing location information for ~w:~w/~w",
		      [?MODULE,F,A]),
	    ok;
	[_|_] ->
	    Line = proplists:get_value(line, Loc),
	    File = proplists:get_value(file, Loc),
	    File = ?MODULE_STRING ++ ".erl"
    end.

process_info_other(Config) when is_list(Config) ->
    Self = self(),
    Pid = spawn_link(fun() -> process_info_looper(Self) end),
    receive after 1 -> ok end,
    pio_current_location(10000, Pid, 0, 0),
    pio_current_stacktrace().

pio_current_location(0, _, Pi, Looper) ->
    io:format("~w call(s) to erlang:process_info/2", [Pi]),
    io:format("~w call(s) to ~w:process_info_looper/1", [Looper,?MODULE]);
pio_current_location(N, Pid, Pi, Looper) ->
    erlang:yield(),
    {current_location,Where} = process_info(Pid, current_location),
    case Where of
	{erlang,process_info,2,[]} ->
	    pio_current_location(N-1, Pid, Pi+1, Looper);
	{erts_internal,await_result,1, Loc} when is_list(Loc) ->
	    pio_current_location(N-1, Pid, Pi+1, Looper);
	{?MODULE,process_info_looper,1,Loc} when is_list(Loc) ->
	    pio_current_location(N-1, Pid, Pi, Looper+1);
	_ ->
	    exit({unexpected_location, Where})
    end.

pio_current_stacktrace() ->
    L = [begin
	     case process_info(P, current_stacktrace) of
                 {current_stacktrace, Stk} -> {P,Stk};
                 undefined -> {P, []}
             end
	 end || P <- processes()],
    [erlang:garbage_collect(P) || {P,_} <- L],
    erlang:garbage_collect(),
    [verify_stacktrace(Stk) || {_,Stk} <- L],
    ok.

verify_stacktrace([{M,F,A,Loc}|T])
  when is_atom(M),
       is_atom(F),
       is_integer(A),
       is_list(Loc) ->
    verify_stacktrace(T);
verify_stacktrace([]) -> ok.

process_info_looper(Parent) ->
    process_info(Parent, current_location),
    process_info_looper(Parent).

%% Tests the process_info/1 BIF on another process with messages.
process_info_other_msg(Config) when is_list(Config) ->
    Self = self(),
    Pid = spawn_link(fun() -> other_process(Self) end),
    receive
	{go_ahead,Pid} -> ok
    end,

    Own = {my,own,message},

    {messages,[Own]} = process_info(Pid, messages),
    
    Garbage = kb_128(),
    MsgA = {a,Garbage},
    MsgB = {b,Garbage},
    MsgC = {c,Garbage},
    MsgD = {d,Garbage},
    MsgE = {e,Garbage},

    Pid ! MsgA,
    {messages,[Own,MsgA]} = process_info(Pid, messages),
    Pid ! MsgB,
    {messages,[Own,MsgA,MsgB]} = process_info(Pid, messages),
    Pid ! MsgC,
    {messages,[Own,MsgA,MsgB,MsgC]} = process_info(Pid, messages),
    Pid ! MsgD,
    {messages,[Own,MsgA,MsgB,MsgC,MsgD]} = process_info(Pid, messages),
    Pid ! MsgE,
    {messages,[Own,MsgA,MsgB,MsgC,MsgD,MsgE]=All} = process_info(Pid, messages),
    {memory,BytesOther} = process_info(Pid, memory),
    {memory,BytesSelf} = process_info(self(), memory),

    io:format("Memory ~p: ~p\n", [Pid,BytesOther]),
    io:format("Memory ~p (self): ~p\n", [self(),BytesSelf]),

    [Own,MsgA,MsgB,MsgC,MsgD,MsgE] = All,

    Pid ! {self(),empty},
    receive
	      empty -> ok
	  end,
    {messages,[]} = process_info(Pid, messages),

    {min_heap_size, 233} = process_info(Pid, min_heap_size),
    {min_bin_vheap_size, 46422} = process_info(Pid, min_bin_vheap_size),
    {max_heap_size, #{ size := 0, kill := true, error_logger := true,
                       include_shared_binaries := false}} =
        process_info(Pid, max_heap_size),

    Pid ! stop,
    ok.

process_info_other_message_queue_len_signal_race(Config) when is_list(Config) ->
    %% OTP-18169
    %%
    %% The race window triggering this bug is quite small. This test
    %% wont fail even with the bug present, but it may trigger an
    %% assertion in the debug compiled emulator if the bug is
    %% present...
    process_flag(priority, high),
    SSchdlr = case erlang:system_info(schedulers_online) of
                  1 -> 1;
                  _ -> 2
              end,
    Flush = fun Flush () ->
                    receive _ -> Flush()
                    after 0 -> ok
                    end
            end,
    RFun = fun RFun () ->
                   receive
                       {flush, From} ->
                           Flush(),
                           From ! flushed
                   end,
                   RFun()
           end,
    R = spawn_opt(RFun, [link,
                         {scheduler, 1},
                         {message_queue_data, on_heap}]),
    SFun = fun SFun () ->
                   receive go -> ok end,
                   M = erlang:monitor(process, R),
                   R ! hi,
                   receive
                       {demonitor, From} ->
                           _ = erlang:demonitor(M),
                           From ! demonitored
                   end,
                   SFun()
           end,
    S = spawn_opt(SFun, [link,
                         {scheduler, SSchdlr},
                         {priority, high}]),
    process_info_other_message_queue_len_signal_race_test(10000, S, R),
    unlink(R),
    exit(R, kill),
    unlink(S),
    exit(S, kill),
    false = is_process_alive(R),
    false = is_process_alive(S),
    ok.

process_info_other_message_queue_len_signal_race_test(0, _S, _R) ->
    ok;
process_info_other_message_queue_len_signal_race_test(N, S, R) ->
    S ! go,
    erlang:yield(),
    _ = process_info(R, message_queue_len),
    S ! {demonitor, self()},
    receive demonitored -> ok end,
    R ! {flush, self()},
    receive flushed -> ok end,
    process_info_other_message_queue_len_signal_race_test(N-1, S, R).

process_info_other_dist_msg(Config) when is_list(Config) ->
    %%
    %% Check that process_info can handle messages that have not been
    %% decoded yet.
    %%
    {ok, Peer, Node} = ?CT_PEER(),
    Self = self(),
    Pid = spawn_link(fun() -> other_process(Self) end),
    receive {go_ahead,Pid} -> ok end,

    Own = {my,own,message},

    {messages,[Own]} = process_info(Pid, messages),
    Garbage = kb_128(),
    MsgA = {a,self(),Garbage},
    MsgB = {b,self(),Garbage},
    MsgC = {c,self(),Garbage},
    MsgD = {d,self(),Garbage},
    MsgE = {e,self(),Garbage},

    %% We don't want the other process to decode messages itself
    %% therefore we suspend it.
    true =  erlang:suspend_process(Pid),
    spawn_link(Node, fun () ->
		Pid  ! MsgA,
		Pid  ! MsgB,
		Pid  ! MsgC,
		Self ! check_abc
	end),
    receive check_abc -> ok end,
    [{status,suspended},
	{messages,[Own,MsgA,MsgB,MsgC]},
	{status,suspended}]= process_info(Pid, [status,messages,status]),
    spawn_link(Node, fun () ->
		Pid  ! MsgD,
		Pid  ! MsgE,
		Self ! check_de
	end),
    receive check_de -> ok end,
    {messages,[Own,MsgA,MsgB,MsgC,MsgD,MsgE]=All} = process_info(Pid, messages),
    true = erlang:resume_process(Pid),
    Pid ! {self(), get_all_messages},
    receive
	      {all_messages, AllMsgs} ->
		  All = AllMsgs
	  end,
    {messages,[]} = process_info(Pid, messages),
    Pid ! stop,
    stop_node(Peer, Node),
    ok.

process_info_other_status(Config) when is_list(Config) ->
    %% OTP-17628: status was erroneously reported as 'running',
    %% when it should be 'waiting', when the priority of the
    %% caller exceeded the priority of the processes being
    %% checked (due to prio elevation).
    Self = self(),
    Other = spawn_link(fun () -> other_process(Self) end),
    receive {go_ahead, Other} -> ok end,
    receive after 100 -> ok end,
    {status, waiting} = process_info(Other, status),
    process_flag(priority, high),
    {status, waiting} = process_info(Other, status),
    process_flag(priority, max),
    {status, waiting} = process_info(Other, status),
    Other ! stop,
    ok.

other_process(Parent) ->
    self() ! {my,own,message},
    Parent ! {go_ahead,self()},
    other_process_1().

other_process_1() ->
    receive
	{Parent,get_all_messages} ->
	    Parent ! {all_messages, get_all_messages()},
	    other_process_1();
	{Parent,empty} ->
	    receive_all(),
	    Parent ! empty,
	    other_process_1();
	stop -> ok
    end.

get_all_messages() ->
    get_all_messages([]).

get_all_messages(Msgs) ->
    receive
	Msg ->
	    get_all_messages([Msg|Msgs])
    after 0 ->
	    lists:reverse(Msgs)
    end.

receive_all() ->
    receive
	_ -> receive_all()
    after 0 -> ok
    end.

chk_pi_order([],[]) ->
    ok;
chk_pi_order([{Arg, _}| Values], [Arg|Args]) ->
    chk_pi_order(Values, Args).

process_info_2_list(Config) when is_list(Config) ->
    Proc = spawn_link(fun () -> receive after infinity -> ok end end),
    register(process_SUITE_process_info_2_list1, self()),
    register(process_SUITE_process_info_2_list2, Proc),
    erts_debug:set_internal_state(available_internal_state,true),
    AllArgs = erts_debug:get_internal_state(process_info_args),
    A1 = lists:sort(AllArgs) ++ [status] ++ lists:reverse(AllArgs),

    %% Verify that argument is accepted as single atom
    lists:foreach(fun (A) ->
		{A, _} = process_info(Proc, A),
		{A, _} = process_info(self(), A)
	end, A1),

    %% Verify that order is preserved
    ok = chk_pi_order(process_info(self(), A1), A1),
    ok = chk_pi_order(process_info(Proc, A1), A1),

    %% Small arg list
    A2 = [status, stack_size, trap_exit, priority],
    [{status, _}, {stack_size, _}, {trap_exit, _}, {priority, _}]
	= process_info(Proc, A2),
    [{status, _}, {stack_size, _}, {trap_exit, _}, {priority, _}]
	= process_info(self(), A2),

    %% Huge arg list (note values are shared)
    A3 = lists:duplicate(5000,backtrace),
    V3 = process_info(Proc, A3),
    5000 = length(V3),
    lists:foreach(fun ({backtrace, _}) -> ok end, V3),
    ok.
    
process_info_lock_reschedule(Config) when is_list(Config) ->
    %% We need a process that is running and an item that requires
    %% process_info to take the main process lock.
    Target1 = spawn_link(fun tok_loop/0),
    Name1 = process_info_lock_reschedule_running,
    register(Name1, Target1),
    Target2 = spawn_link(fun () -> receive after infinity -> ok end end),
    Name2 = process_info_lock_reschedule_waiting,
    register(Name2, Target2),
    PI = fun(_) ->
	    erlang:yield(),
	    [{registered_name, Name1}] = process_info(Target1, [registered_name]),
	    [{registered_name, Name2}] = process_info(Target2, [registered_name]),
	    erlang:yield(),
	    {registered_name, Name1} = process_info(Target1, registered_name),
	    {registered_name, Name2} = process_info(Target2, registered_name),
	    erlang:yield(),
	    [{registered_name, Name1}| _] = process_info(Target1),
	    [{registered_name, Name2}| _] = process_info(Target2)
    end,
    lists:foreach(PI, lists:seq(1,1000)),
    %% Make sure Target1 still is willing to "tok loop"
    case process_info(Target1, status) of
	{status, OkStatus} when OkStatus == runnable;
				OkStatus == running;
				OkStatus == garbage_collecting ->
	    unlink(Target1),
	    unlink(Target2),
	    exit(Target1, bang),
	    exit(Target2, bang),
	    OkStatus;
	{status, BadStatus} ->
	    ct:fail(BadStatus)
    end.

pi_loop(_Name, _Pid, 0) ->
    ok;
pi_loop(Name, Pid, N) ->
    {registered_name, Name} = process_info(Pid, registered_name),
    pi_loop(Name, Pid, N-1).

process_info_lock_reschedule2(Config) when is_list(Config) ->
    Parent = self(),
    Fun = fun () ->
	    receive {go, Name, Pid} -> ok end,
	    pi_loop(Name, Pid, 10000),
	    Parent ! {done, self()},
	    receive after infinity -> ok end
    end,
    P1 = spawn_link(Fun),
    N1 = process_info_lock_reschedule2_1,
    true = register(N1, P1),
    P2 = spawn_link(Fun),
    N2 = process_info_lock_reschedule2_2,
    true = register(N2, P2),
    P3 = spawn_link(Fun),
    N3 = process_info_lock_reschedule2_3,
    true = register(N3, P3),
    P4 = spawn_link(Fun),
    N4 = process_info_lock_reschedule2_4,
    true = register(N4, P4),
    P5 = spawn_link(Fun),
    N5 = process_info_lock_reschedule2_5,
    true = register(N5, P5),
    P6 = spawn_link(Fun),
    N6 = process_info_lock_reschedule2_6,
    true = register(N6, P6),
    P1 ! {go, N2, P2},
    P2 ! {go, N1, P1},
    P3 ! {go, N1, P1},
    P4 ! {go, N1, P1},
    P5 ! {go, N6, P6},
    P6 ! {go, N5, P5},
    receive {done, P1} -> ok end,
    receive {done, P2} -> ok end,
    receive {done, P3} -> ok end,
    receive {done, P4} -> ok end,
    receive {done, P5} -> ok end,
    receive {done, P6} -> ok end,
    unlink(P1), exit(P1, bang),
    unlink(P2), exit(P2, bang),
    unlink(P3), exit(P3, bang),
    unlink(P4), exit(P4, bang),
    unlink(P5), exit(P5, bang),
    unlink(P6), exit(P6, bang),
    ok.

many_args(0,_B,_C,_D,_E,_F,_G,_H,_I,_J) ->
    ok;
many_args(A,B,C,D,E,F,G,H,I,J) ->
    many_args(A-1,B,C,D,E,F,G,H,I,J).

do_pi_msg_len(PT, AT) ->
    lists:map(fun (_) -> ok end, [a,b,c,d]),
    {message_queue_len, _} = process_info(element(2,PT), element(2,AT)).
    
process_info_lock_reschedule3(Config) when is_list(Config) ->
    %% We need a process that is running and an item that requires
    %% process_info to take the main process lock.
    Target1 = spawn_link(fun tok_loop/0),
    Name1 = process_info_lock_reschedule_running,
    register(Name1, Target1),
    Target2 = spawn_link(fun () -> receive after infinity -> ok end end),
    Name2 = process_info_lock_reschedule_waiting,
    register(Name2, Target2),
    PI = fun(N) ->
	    case N rem 10 of
		0 -> erlang:yield();
		_ -> ok
	    end,
	    do_pi_msg_len({proc, Target1},
		{arg, message_queue_len})
    end,
    many_args(100000,1,2,3,4,5,6,7,8,9),
    lists:foreach(PI, lists:seq(1,1000000)),
    %% Make sure Target1 still is willing to "tok loop"
    case process_info(Target1, status) of
	      {status, OkStatus} when OkStatus == runnable;
				      OkStatus == running;
				      OkStatus == garbage_collecting ->
		  unlink(Target1),
		  unlink(Target2),
		  exit(Target1, bang),
		  exit(Target2, bang),
		  OkStatus;
	      {status, BadStatus} ->
		  ct:fail(BadStatus)
	  end.

otp_4725(Config) when is_list(Config) ->
    Tester = self(),
    Ref1 = make_ref(),
    Pid1 = spawn_opt(fun () ->
		Tester ! {Ref1, process_info(self())},
		receive
		    Ref1 -> bye
		end
	end, [link, {priority, max}, {fullsweep_after, 600}]),
    receive
	{Ref1, ProcInfo1A} ->
	    ProcInfo1B = process_info(Pid1),
	    Pid1 ! Ref1,
	    check_proc_infos(ProcInfo1A, ProcInfo1B)
    end,
    Ref2 = make_ref(),
    Pid2 = spawn_opt(fun () ->
		Tester ! {Ref2, process_info(self())},
		receive
		    Ref2 -> bye
		end
	end,
	[]),
    receive
	{Ref2, ProcInfo2A} ->
	    ProcInfo2B = process_info(Pid2),
	    Pid2 ! Ref2,
	    check_proc_infos(ProcInfo2A, ProcInfo2B)
    end,
    ok.

check_proc_infos(A, B) ->
    IC = lists:keysearch(initial_call, 1, A),
    IC = lists:keysearch(initial_call, 1, B),

    L = lists:keysearch(links, 1, A),
    L = lists:keysearch(links, 1, B),

    D = lists:keysearch(dictionary, 1, A),
    D = lists:keysearch(dictionary, 1, B),

    TE = lists:keysearch(trap_exit, 1, A),
    TE = lists:keysearch(trap_exit, 1, B),

    EH = lists:keysearch(error_handler, 1, A),
    EH = lists:keysearch(error_handler, 1, B),

    P = lists:keysearch(priority, 1, A),
    P = lists:keysearch(priority, 1, B),

    GL = lists:keysearch(group_leader, 1, A),
    GL = lists:keysearch(group_leader, 1, B),

    GC = lists:keysearch(garbage_collection, 1, A),
    GC = lists:keysearch(garbage_collection, 1, B),
    {value, {garbage_collection, GClist}} = GC,

    %% This is not really documented
    true = is_integer(gv(minor_gcs, GClist)),
    true = is_integer(gv(fullsweep_after, GClist)),
    true = is_integer(gv(min_heap_size, GClist)),
    #{error_logger := Bool1,
      include_shared_binaries := Bool2,
      kill := Bool3,
      size := MaxHeapSize} = gv(max_heap_size, GClist),
    true = is_boolean(Bool1),
    true = is_boolean(Bool2),
    true = is_boolean(Bool3),
    true = is_integer(MaxHeapSize),

    ok.


%% Dummies.

start_spawner() ->
    ok.

stop_spawner() ->
    ok.

%% Tests erlang:process_info(Pid, garbage_collection_info)
process_info_garbage_collection(_Config) ->
    Parent = self(),
    Pid = spawn_link(
            fun() ->
                    %% We set mqd to off_heap and send an tuple
                    %% to process in order to force mbuf_size
                    %% to be used
                    process_flag(message_queue_data, off_heap),
                    receive go -> ok end,
                    (fun F(0) ->
                             Parent ! deep,
                             receive {ok,_} -> ok end,
                             [];
                         F(N) ->
                             timer:sleep(1),
                             [lists:seq(1,100) | F(N-1)]
                     end)(1000),
                    Parent ! shallow,
                    receive done -> ok end
            end),
    [{garbage_collection_info, Before},{total_heap_size, THSBefore}] =
        erlang:process_info(Pid, [garbage_collection_info, total_heap_size]),
    Pid ! go, receive deep -> ok end,
    [{_, Deep},{_,THSDeep}]  =
         erlang:process_info(Pid, [garbage_collection_info, total_heap_size]),
    Pid ! {ok, make_ref()}, receive shallow -> ok end,
    [{_, After},{_, THSAfter}] =
        erlang:process_info(Pid, [garbage_collection_info, total_heap_size]),
    Pid ! done,

    %% Do some general checks to see if everything seems to be roughly correct
    ct:log("Before: ~p",[Before]),
    ct:log("Deep: ~p",[Deep]),
    ct:log("After: ~p",[After]),
    ct:log("Before THS: ~p",[THSBefore]),
    ct:log("Deep THS: ~p",[THSDeep]),
    ct:log("After THS: ~p",[THSAfter]),

    %% Check stack_size
    true = gv(stack_size, Before) < gv(stack_size, Deep),
    true = gv(stack_size, After) < gv(stack_size, Deep),

    %% Check used heap size
    true = gv(heap_size, Before) + gv(old_heap_size, Before)
        < gv(heap_size, Deep) + gv(old_heap_size, Deep),
    true = gv(heap_size, Before) + gv(old_heap_size, Before)
        < gv(heap_size, After) + gv(old_heap_size, After),

    %% Check that total_heap_size == heap_block_size + old_heap_block_size + mbuf_size
    THSBefore = gv(heap_block_size, Before)
        + gv(old_heap_block_size, Before)
        + gv(mbuf_size, Before),

    THSDeep = gv(heap_block_size, Deep)
        + gv(old_heap_block_size, Deep)
        + gv(mbuf_size, Deep),

    THSAfter = gv(heap_block_size, After)
        + gv(old_heap_block_size, After)
        + gv(mbuf_size, After),

    ok.

gv(Key,List) ->
    proplists:get_value(Key,List).

process_info_parent(Config) when is_list(Config) ->
    Child = spawn_link(fun () -> receive stop -> ok end end),
    ?assertEqual({parent, self()}, erlang:process_info(Child, parent)),
    Child ! stop,
    ?assertEqual({parent, undefined}, erlang:process_info(whereis(init), parent)),

    {ok, Peer, Node} = ?CT_PEER(),
    RemoteChild = spawn_link(Node,
                             fun () ->
                                     {parent, Parent} = process_info(self(), parent),
                                     garbage_collect(),
                                     {parent, Parent} = process_info(self(), parent),
                                     garbage_collect(),
                                     Parent ! remote_child_hello,
                                     receive stop -> ok end
                             end),
    ?assertEqual({parent, self()},
                 erpc:call(Node, erlang, process_info, [RemoteChild, parent])),
    receive remote_child_hello -> ok end,
    unlink(RemoteChild),
    RemoteChild ! stop,
    peer:stop(Peer),
    ok.

process_info_smoke_all_tester() ->
    register(process_info_smoke_all_tester, self()),
    put(ets_ref, ets:new(blupp, [])),
    put(binary, [list_to_binary(lists:duplicate(1000, 1)),
                 list_to_binary(lists:duplicate(1000, 2))]),
    process_info_smoke_all_tester_loop().

process_info_smoke_all_tester_loop() ->
    receive
        {other_process, Pid} ->
            case get(procs) of
                undefined -> put(procs, [Pid]);
                Procs -> put(procs, [Pid|Procs])
            end,
            erlang:monitor(process, Pid),
            link(Pid),
            process_info_smoke_all_tester_loop()
    end.

process_info_smoke_all(Config) when is_list(Config) ->
    AllPIOptions = [registered_name,
                    current_function,
                    initial_call,
                    messages,
                    message_queue_len,
                    links,
                    monitors,
                    monitored_by,
                    dictionary,
                    trap_exit,
                    error_handler,
                    heap_size,
                    stack_size,
                    memory,
                    garbage_collection,
                    group_leader,
                    reductions,
                    priority,
                    trace,
                    binary,
                    sequential_trace_token,
                    catchlevel,
                    backtrace,
                    last_calls,
                    total_heap_size,
                    suspending,
                    min_heap_size,
                    min_bin_vheap_size,
                    max_heap_size,
                    current_location,
                    current_stacktrace,
                    message_queue_data,
                    garbage_collection_info,
                    magic_ref,
                    fullsweep_after,
                    {dictionary, ets_ref}],

    {ok, Peer, Node} = ?CT_PEER(),
    RP = spawn_link(Node, fun process_info_smoke_all_tester/0),
    LP = spawn_link(fun process_info_smoke_all_tester/0),
    RP ! {other_process, LP},
    LP ! {other_process, RP},
    LP ! {other_process, self()},
    LP ! ets:new(blapp, []),
    LP ! ets:new(blipp, []),
    LP ! list_to_binary(lists:duplicate(1000, 3)),
    receive after 1000 -> ok end,
    _MLP = erlang:monitor(process, LP),
    true = is_process_alive(LP),
    PI = process_info(LP, AllPIOptions),
    io:format("~p~n", [PI]),
    garbage_collect(),
    unlink(RP),
    unlink(LP),
    exit(RP, kill),
    exit(LP, kill),
    false = is_process_alive(LP),
    stop_node(Peer, Node),
    ok.

process_info_status_handled_signal(Config) when is_list(Config) ->
    P = spawn_link(fun () ->
                           receive after infinity -> ok end
                   end),
    wait_until(fun () ->
                       process_info(P, status) == {status, waiting}
               end),
    %%
    %% The 'messages' option will force a process-info-request
    %% signal to be scheduled on the process. Ensure that status
    %% 'waiting' is reported even though it is actually running
    %% when handling the request. We want it to report the status
    %% it would have had if it had not been handling the
    %% process-info-request...
    %%
    [{status, waiting}, {messages, []}] = process_info(P, [status, messages]),
    unlink(P),
    exit(P, kill),
    false = erlang:is_process_alive(P),
    ok.

%% OTP-15709
%% Provoke a bug where process_info(reductions) returned wrong result
%% because REDS_IN (def_arg_reg[5]) is read when the process in not running.
%%
%% And a bug where process_info(reductions) on a process which was releasing its
%% main lock during execution could result in negative reduction diffs.
process_info_reductions(Config) when is_list(Config) ->
    {S1, S2} = case erlang:system_info(schedulers_online) of
                   1 -> {1,1};
                   _ -> {1,2}
               end,
    io:format("Run on schedulers ~p and ~p\n", [S1,S2]),
    Boss = self(),
    Doer = spawn_opt(fun () ->
                             pi_reductions_tester(true, 10, fun pi_reductions_spinnloop/0, S2),
                             pi_reductions_tester(true, 10, fun pi_reductions_recvloop/0, S2),
                             pi_reductions_tester(false, 100, fun pi_reductions_main_unlocker/0, S2),
                             Boss ! {self(), done}
                     end,
                     [link, {scheduler, S1}]),

    {Doer, done} = receive M -> M end,
    ok.

pi_reductions_tester(ForceSignal, MaxCalls, Fun, S2) ->
    Pid = spawn_opt(Fun, [link, {scheduler,S2}]),
    Extra = case ForceSignal of
                true ->
                    %% Add another item that force sending the request
                    %% as a signal, like 'current_function'.
                    [current_function];
                false ->
                    []
            end,
    LoopFun = fun Me(Calls, Prev, Acc0) ->
                      PI = process_info(Pid, [reductions | Extra]),
                      [{reductions,Reds} | _] = PI,
                      Diff = Reds - Prev,
                      %% Verify we get sane non-negative reduction diffs
                      {Diff, true} = {Diff, (Diff >= 0)},
                      {Diff, true} = {Diff, (Diff =< 1000*1000)},
                      Acc1 = [Diff | Acc0],
                      case Calls >= MaxCalls of
                          true -> Acc1;
                          false -> Me(Calls+1, Reds, Acc1)
                      end
              end,
    DiffList = LoopFun(0, 0, []),
    unlink(Pid),
    exit(Pid,kill),
    io:format("Reduction diffs: ~p\n", [lists:reverse(DiffList)]),
    ok.

pi_reductions_spinnloop() ->
    %% 6 args to make use of def_arg_reg[5] which is also used as REDS_IN
    pi_reductions_spinnloop(999*1000, atom, "hej", self(), make_ref(), 3.14).

pi_reductions_spinnloop(N,A,B,C,D,E) when N > 0 ->
    pi_reductions_spinnloop(N-1,B,C,D,E,A);
pi_reductions_spinnloop(0,_,_,_,_,_) ->
    %% Stop to limit max number of reductions consumed
    pi_reductions_recvloop().

pi_reductions_recvloop() ->
    receive
        "a free lunch" -> false
    end.

pi_reductions_main_unlocker() ->
    Other = spawn_link(fun() -> receive die -> ok end end),
    pi_reductions_main_unlocker_loop(Other).

pi_reductions_main_unlocker_loop(Other) ->
    %% Assumption: register(OtherPid, Name) will unlock main lock of calling
    %% process during execution.
    register(pi_reductions_main_unlocker, Other),
    unregister(pi_reductions_main_unlocker),

    %% Yield in order to increase probability of process_info sometimes probing
    %% this process when it's not RUNNING.
    erlang:yield(),
    pi_reductions_main_unlocker_loop(Other).

process_info_self_signal(Config) when is_list(Config) ->
    %% Test that signals that we have sent to ourselves are
    %% visible in process_info() result. This is not strictly
    %% a necessary property, but implemented so now. See
    %% process_info.c:process_info_bif() for more info.
    Self = self(),
    Ref = make_ref(),
    pi_sig_spam_test(fun () ->
                             process_info_self_signal_spammer(Self)
                     end,
                     fun () ->
                             self() ! Ref,
                             process_info(self(), messages)
                     end,
                     fun (Res) ->
                             {messages, [Ref]} = Res
                     end).

process_info_self_signal_spammer(To) ->
    erlang:demonitor(erlang:monitor(process, To)),
    process_info_self_signal_spammer(To).

process_info_self_msgq_len(Config) when is_list(Config) ->
    %% Spam ourselves with signals forcing us to flush own
    %% signal queue..
    Self = self(),
    pi_sig_spam_test(fun () ->
                             process_info_self_msgq_len_spammer(Self)
                     end,
                     fun () ->
                             process_info(self(), message_queue_len)
                     end,
                     fun (Res) ->
                             {message_queue_len, Len} = Res,
                             true = Len > 0,
                             ok
                     end).


process_info_self_msgq_len_messages(Config) when is_list(Config) ->
    %% Spam ourselves with signals normally forcing us to flush own
    %% signal queue, but since we also want messages wont be flushed...
    Self = self(),
    pi_sig_spam_test(fun () ->
                             process_info_self_msgq_len_spammer(Self, 100000)
                     end,
                     fun () ->
                             process_info(self(),
                                          [message_queue_len,
                                           messages])
                     end,
                     fun (Res) ->
                             [{message_queue_len, Len},
                              {messages, Msgs}] = Res,
                             Len = length(Msgs),
                             ok
                     end).

process_info_self_msgq_len_more(Config) when is_list(Config) ->
    self() ! hej,
    BodyRes = process_info_self_msgq_len_more_caller_body(),
    ok = process_info_self_msgq_len_more_caller_body_result(BodyRes),
    TailRes = process_info_self_msgq_len_more_caller_tail(),
    ok = process_info_self_msgq_len_more_caller_tail_result(TailRes),
    receive hej -> ok end,
    %% Check that current_function, current_location, and
    %% current_stacktrace give sane results flushing or not...
    Self = self(),
    pi_sig_spam_test(fun () ->
                             process_info_self_msgq_len_spammer(Self)
                     end,
                     fun process_info_self_msgq_len_more_caller_body/0,
                     fun process_info_self_msgq_len_more_caller_body_result/1),
    pi_sig_spam_test(fun () ->
                             process_info_self_msgq_len_spammer(Self)
                     end,
                     fun process_info_self_msgq_len_more_caller_tail/0,
                     fun process_info_self_msgq_len_more_caller_tail_result/1).

process_info_self_msgq_len_more_caller_body() ->
    Res = process_info(self(),
                       [message_queue_len,
                        current_function,
                        current_location,
                        current_stacktrace]),
    id(Res).

process_info_self_msgq_len_more_caller_body_result(Res) ->
    [{message_queue_len, Len},
     {current_function, {process_SUITE,process_info_self_msgq_len_more_caller_body,0}},
     {current_location, {process_SUITE,process_info_self_msgq_len_more_caller_body,0,_}},
     {current_stacktrace,
      [{process_SUITE,process_info_self_msgq_len_more_caller_body,0,_} | _]}] = Res,
    true = Len > 0,
    ok.

process_info_self_msgq_len_more_caller_tail() ->
    process_info(self(),
                 [message_queue_len,
                  current_function,
                  current_location,
                  current_stacktrace]).

process_info_self_msgq_len_more_caller_tail_result(Res) ->
    [{message_queue_len, Len},
     {current_function, {process_SUITE,process_info_self_msgq_len_more_caller_tail,0}},
     {current_location, {process_SUITE,process_info_self_msgq_len_more_caller_tail,0,_}},
     {current_stacktrace,
      [{process_SUITE,process_info_self_msgq_len_more_caller_tail,0,_} | _]}] = Res,
    true = Len > 0,
    ok.
    

process_info_self_msgq_len_spammer(To) ->
    process_info_self_msgq_len_spammer(To, 10000000).

process_info_self_msgq_len_spammer(_To, 0) ->
    ok;
process_info_self_msgq_len_spammer(To, N) ->
    To ! hejhopp,
    erlang:demonitor(erlang:monitor(process, To)),
    process_info_self_msgq_len_spammer(To, N-1).

pi_sig_spam_test(SpamFun, PITest, PICheckRes) ->
    SO = erlang:system_flag(schedulers_online, 1),
    try
        Self = self(),
        SigSpammer = spawn_link(SpamFun),
        process_flag(priority, low),
        receive after 10 -> ok end,
        Res = PITest(),
        process_flag(priority, high),
        unlink(SigSpammer),
        exit(SigSpammer, kill),
        false = is_process_alive(SigSpammer),
        PICheckRes(Res)
    after
        _ = erlang:system_flag(schedulers_online, SO)
    end.

process_info_msgq_len_no_very_long_delay(Config) when is_list(Config) ->
    Tester = self(),
    P1 = spawn_link(fun () ->
                            receive after infinity -> ok end
                    end),
    {message_queue_len, 0} = process_info(self(), message_queue_len),
    {message_queue_len, 0} = process_info(P1, message_queue_len),
    P2 = spawn_link(fun () ->
                            Tester ! hello,
                            P1 ! hello,
                            receive after infinity -> ok end
                    end),
    receive after 100 -> ok end,
    {message_queue_len, 1} = process_info(self(), message_queue_len),
    {message_queue_len, 1} = process_info(P1, message_queue_len),
    receive hello -> ok end,
    {message_queue_len, 0} = process_info(self(), message_queue_len),
    unlink(P1),
    exit(P1, kill),
    unlink(P2),
    exit(P2, kill),
    false = is_process_alive(P1),
    false = is_process_alive(P2),
    ok.

process_info_dict_lookup(Config) when is_list(Config) ->
    Pid = spawn_link(fun proc_dict_helper/0),
    {async_dist, AsyncDist} = process_info(Pid, async_dist),
    Ref = make_ref(),
    Bin = <<17:4096>>,
    Int0 = 9999999999999999999999999999999999,
    Int1 = 1111111111111111111111111111111111,
    Tuple = {make_ref(), erlang:monotonic_time()},

    %% Check that we can lookup dictionary values on another process...
    pdh(Pid, put_async, [hej, hopp]),
    pdh(Pid, put_async, [hopp, hej]),
    pdh(Pid, put_async, [Ref, Int0]),
    pdh(Pid, put_async, [Int0, Int1]),
    pdh(Pid, put_async, [Pid, Ref]),
    pdh(Pid, put_async, [Tuple, Bin]),
    undefined = pdh(Pid, put, [Bin, Ref]),

    erlang:garbage_collect(Pid),

    {{dictionary, Ref}, Int0} = process_info(Pid, {dictionary, Ref}),
    [{{dictionary, Ref}, Int0}] = process_info(Pid, [{dictionary, Ref}]),

    PIRes = process_info(Pid, [async_dist,
                               trap_exit,
                               {dictionary, hej},
                               {dictionary, hopp},
                               {dictionary, Ref},
                               {dictionary, Int0},
                               async_dist,
                               trap_exit,
                               {dictionary, Pid},
                               {dictionary, Tuple},
                               {dictionary, Bin}]),
    ct:log("PIRes = ~p", [PIRes]),
    PIRes = [{async_dist, AsyncDist},
             {trap_exit, false},
             {{dictionary, hej}, hopp},
             {{dictionary, hopp}, hej},
             {{dictionary, Ref}, Int0},
             {{dictionary, Int0}, Int1},
             {async_dist, AsyncDist},
             {trap_exit, false},
             {{dictionary, Pid}, Ref},
             {{dictionary, Tuple}, Bin},
             {{dictionary, Bin}, Ref}],

    pdh(Pid, erase_async, [hej]),
    pdh(Pid, erase_async, [hopp]),
    pdh(Pid, erase_async, [Ref]),
    pdh(Pid, erase_async, [Int0]),
    pdh(Pid, erase_async, [Pid]),
    pdh(Pid, erase_async, [Tuple]),
    Ref = pdh(Pid, erase, [Bin]),

    erlang:garbage_collect(Pid),

    {{dictionary, Ref}, undefined} = process_info(Pid, {dictionary, Ref}),
    [{{dictionary, Ref}, undefined}] = process_info(Pid, [{dictionary, Ref}]),

    PIRes2 = process_info(Pid, [async_dist,
                                trap_exit,
                                {dictionary, hej},
                                {dictionary, hopp},
                                {dictionary, Ref},
                                {dictionary, Int0},
                                async_dist,
                                trap_exit,
                                {dictionary, Pid},
                                {dictionary, Tuple},
                                {dictionary, Bin}]),
    ct:log("PIRes2 = ~p", [PIRes2]),

    PIRes2 = [{async_dist, AsyncDist},
             {trap_exit, false},
             {{dictionary, hej}, undefined},
             {{dictionary, hopp}, undefined},
             {{dictionary, Ref}, undefined},
             {{dictionary, Int0}, undefined},
             {async_dist, AsyncDist},
             {trap_exit, false},
             {{dictionary, Pid}, undefined},
             {{dictionary, Tuple}, undefined},
             {{dictionary, Bin}, undefined}],

    unlink(Pid),
    exit(Pid,kill),

    %% Also check that it works on ourself...

    put(hej, hopp),
    put(hopp, hej),
    put(Ref, Int0),
    put(Int0, Int1),
    put(Pid, Ref),
    put(Tuple, Bin),
    undefined = put(Bin, Ref),

    erlang:garbage_collect(),

    {{dictionary, Ref}, Int0} = process_info(self(), {dictionary, Ref}),
    [{{dictionary, Ref}, Int0}] = process_info(self(), [{dictionary, Ref}]),

    PIRes3 = process_info(self(), [async_dist,
                                   trap_exit,
                                   {dictionary, hej},
                                   {dictionary, hopp},
                                   {dictionary, Ref},
                                   {dictionary, Int0},
                                   async_dist,
                                   trap_exit,
                                   {dictionary, Pid},
                                   {dictionary, Tuple},
                                   {dictionary, Bin}]),
    ct:log("PIRes3 = ~p", [PIRes3]),
    PIRes3 = [{async_dist, AsyncDist},
              {trap_exit, false},
              {{dictionary, hej}, hopp},
              {{dictionary, hopp}, hej},
              {{dictionary, Ref}, Int0},
              {{dictionary, Int0}, Int1},
              {async_dist, AsyncDist},
              {trap_exit, false},
              {{dictionary, Pid}, Ref},
              {{dictionary, Tuple}, Bin},
              {{dictionary, Bin}, Ref}],

    erase(hej),
    erase(hopp),
    erase(Ref),
    erase(Int0),
    erase(Pid),
    erase(Tuple),
    Ref = erase(Bin),

    erlang:garbage_collect(),

    {{dictionary, Ref}, undefined} = process_info(self(), {dictionary, Ref}),
    [{{dictionary, Ref}, undefined}] = process_info(self(), [{dictionary, Ref}]),

    PIRes4 = process_info(self(), [async_dist,
                                   trap_exit,
                                   {dictionary, hej},
                                   {dictionary, hopp},
                                   {dictionary, Ref},
                                   {dictionary, Int0},
                                   async_dist,
                                   trap_exit,
                                   {dictionary, Pid},
                                   {dictionary, Tuple},
                                   {dictionary, Bin}]),
    ct:log("PIRes4 = ~p", [PIRes4]),

    PIRes4 = [{async_dist, AsyncDist},
              {trap_exit, false},
              {{dictionary, hej}, undefined},
              {{dictionary, hopp}, undefined},
              {{dictionary, Ref}, undefined},
              {{dictionary, Int0}, undefined},
              {async_dist, AsyncDist},
              {trap_exit, false},
              {{dictionary, Pid}, undefined},
              {{dictionary, Tuple}, undefined},
              {{dictionary, Bin}, undefined}],

    false = is_process_alive(Pid),
    ok.

process_info_label(Config) when is_list(Config) ->
    Pid = spawn_link(fun proc_dict_helper/0),
    LabelKey = '$process_label',
    Ref = make_ref(),
    Tuple = {make_ref(), erlang:monotonic_time()},

    undefined = pdh(Pid, put, [LabelKey, Tuple]),
    erlang:garbage_collect(Pid),

    {label,Tuple} = process_info(Pid, label),
    Self = self(),
    [{label,Tuple},{registered_name,[]},{links,[Self]}] =
        process_info(Pid, [label,registered_name,links]),

    put(LabelKey, Ref),
    {label,Ref} = process_info(self(), label),

    ok.

pdh(Pid, AsyncOp, Args) when AsyncOp == put_async;
                             AsyncOp == erase_async ->
    Pid ! {AsyncOp, Args},
    ok;
pdh(Pid, SyncOp, Args) ->
    Ref = make_ref(),
    Pid ! {SyncOp, self(), Ref, Args},
    receive {Ref, Res} -> Res end.

proc_dict_helper() ->
    receive
        {put, From, Ref, [Key, Value]} ->
            From ! {Ref, put(Key, Value)};
        {get, From, Ref, [Key]} ->
            From ! {Ref, get(Key)};
        {get, From, Ref, []} ->
            From ! {Ref, get()};
        {erase, From, Ref, [Key]} ->
            From ! {Ref, erase(Key)};
        {put_async, [Key, Value]} ->
            _ = put(Key, Value);
        {erase_async, [Key]} ->
            _ = erase(Key)
    end,
    proc_dict_helper().

suspend_process_pausing_proc_timer(_Config) ->
    BeforeSuspend = fun(_Pid) -> ok end,
    AfterResume = fun(_Pid) -> ok end,
    suspend_process_pausing_proc_timer_aux(BeforeSuspend, AfterResume),
    ok.

suspend_process_pausing_proc_timer_aux(BeforeSuspend, AfterResume) ->
    TcProc = self(),
    Pid = erlang:spawn_link(
        fun() ->
            TcProc ! {sync, self()},
            receive go -> ok
            after 2_000 -> exit(timer_not_paused)
            end,
            TcProc ! {sync, self()},
            receive _ -> error(unexpected)
            after 2_000 -> ok
            end,
            TcProc ! {sync, self()}
        end
    ),

    WaitForSync = fun () ->
        receive {sync, Pid} -> ok
        after 10_000 -> error(timeout)
        end
    end,
    EnsureWaiting = fun() ->
        wait_until(fun () -> process_info(Pid, status) == {status, waiting} end)
    end,

    WaitForSync(),
    EnsureWaiting(),

    BeforeSuspend(Pid),
    true = erlang:suspend_process(Pid),
    timer:sleep(5_000),
    true = erlang:resume_process(Pid),
    AfterResume(Pid),
    timer:sleep(1_000),
    Pid ! go,

    WaitForSync(),
    EnsureWaiting(),

    BeforeSuspend(Pid),
    true = erlang:suspend_process(Pid),
    true = erlang:resume_process(Pid),
    AfterResume(Pid),
    WaitForSync(),
    ok.

%% Tests erlang:bump_reductions/1.
bump_reductions(Config) when is_list(Config) ->
    erlang:garbage_collect(),
    erlang:yield(),		% Clear reductions.
    {reductions,R1} = process_info(self(), reductions),
    true = erlang:bump_reductions(100),
    {reductions,R2} = process_info(self(), reductions),
    case R2-R1 of
	      Diff when Diff < 100 ->
		  ok = io:format("R1 = ~w, R2 = ~w", [R1, R2]),
		  ct:fail({small_diff, Diff});
	      Diff when Diff > 110 ->
		  ok = io:format("R1 = ~w, R2 = ~w", [R1, R2]),
		  ct:fail({big_diff, Diff});
	      Diff ->
		  io:format("~p\n", [Diff]),
		  ok
	  end,

    %% Make sure that a bignum reduction doesn't crash the emulator (32-bit CPU).
    bump_big(R2, 16#08000000).

bump_big(Prev, Limit) ->
    true = erlang:bump_reductions(100000), %Limited to CONTEXT_REDUCTIONS.
    case process_info(self(), reductions) of
	      {reductions,Big} when is_integer(Big), Big > Limit ->
		  erlang:garbage_collect(),
		  io:format("~p\n", [Big]);
	      {reductions,R} when is_integer(R), R > Prev ->
		  bump_big(R, Limit)
	  end,
    ok.

%% Priority 'low' should be mixed with 'normal' using a factor of
%% about 8. (OTP-2644)
low_prio(Config) when is_list(Config) ->
    erlang:system_flag(multi_scheduling, block_normal),
    Prop = low_prio_test(Config),
    erlang:system_flag(multi_scheduling, unblock_normal),
    Str = lists:flatten(io_lib:format("Low/high proportion is ~.3f",
                                      [Prop])),
    {comment,Str}.

low_prio_test(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    %% Spawn the server running with high priority. The server must
    %% not run at normal priority as that would skew the results for
    %% two reasons:
    %%
    %% 1. There would be one more normal-priority processes than
    %% low-priority processes.
    %%
    %% 2. The receive queue would grow faster than the server process
    %% could process it. That would in turn trigger the reduction
    %% punishment for the clients.
    S = spawn_opt(?MODULE, prio_server, [0, 0], [link,{priority,high}]),

    %% Spawn the clients and let them run for a while.
    PCs = spawn_prio_clients(S, erlang:system_info(schedulers_online)),
    ct:sleep({seconds,2}),
    lists:foreach(fun (P) -> exit(P, kill) end, PCs),

    %% Stop the server and retrieve the result.
    S ! exit,
    receive
        {'EXIT', S, {A, B}} ->
            check_prio(A, B)
    end.

check_prio(A, B) ->
    Prop = A/B,
    ok = io:format("Low=~p, High=~p, Prop=~p\n", [A, B, Prop]),

    %% Prop is expected to be appr. 1/8. Allow a reasonable margin.
    true = Prop < 1/4,
    true = Prop > 1/16,
    Prop.

prio_server(A, B) ->
    receive
	low ->
	    prio_server(A+1, B);
	normal ->
	    prio_server(A, B+1);
	exit ->
	    exit({A, B})
    end.

spawn_prio_clients(_, 0) ->
    [];
spawn_prio_clients(S, N) ->
    [spawn_opt(?MODULE, prio_client, [S, normal], [link, {priority,normal}]),
     spawn_opt(?MODULE, prio_client, [S, low], [link, {priority,low}])
     | spawn_prio_clients(S, N-1)].

prio_client(S, Prio) ->
    S ! Prio,
    prio_client(S, Prio).

make_sub_binary(Bin) when is_binary(Bin) ->
    {_,B} = split_binary(list_to_binary([0,1,3,Bin]), 3),
    B;
make_sub_binary(List) ->
    make_sub_binary(list_to_binary(List)).

make_unaligned_sub_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

%% Tests erlang:yield/1
yield(Config) when is_list(Config) ->
    case catch erlang:system_info(modified_timing_level) of
	Level when is_integer(Level) ->
	    {skipped,
	     "Modified timing (level " ++ integer_to_list(Level)
	     ++ ") is enabled. Testcase gets messed up by modified "
	     "timing."};
	_ ->
	    MS = erlang:system_flag(multi_scheduling, block_normal),
	    yield_test(),
	    erlang:system_flag(multi_scheduling, unblock_normal),
	    case MS of
		blocked ->
		    {comment,
		     "Multi-scheduling blocked during test. This test-case "
		     "was not written to work with multiple schedulers (the "
		     "yield2 test-case tests almost the same thing)."};
		_ ->
		    ok
	    end
    end.

yield_test() ->
    erlang:garbage_collect(),
    receive after 1 -> ok end,		% Clear reductions.
    SC = schedcnt(start),
    {reductions, R1} = process_info(self(), reductions),
    {ok, true} = call_yield(middle),
    true = call_yield(final),
    true = call_yield(),
    true = apply(erlang, yield, []),
    {reductions, R2} = process_info(self(), reductions),
    Schedcnt = schedcnt(stop, SC),
    case {R2-R1, Schedcnt} of
	{Diff, 4} when Diff < 30 ->
	    ok = io:format("R1 = ~w, R2 = ~w, Schedcnt = ~w", 
		[R1, R2, Schedcnt]);
	{Diff, _} ->
	    ok = io:format("R1 = ~w, R2 = ~w, Schedcnt = ~w", 
		[R1, R2, Schedcnt]),
	    ct:fail({measurement_error, Diff, Schedcnt})
    end.

call_yield() ->
    erlang:yield().

call_yield(middle) ->
    {ok, erlang:yield()};
call_yield(final) ->
    case self() of
	Self when is_pid(Self) ->
	    ok
    end,
    erlang:yield().

schedcnt(start) ->
    Ref = make_ref(),
    Fun = 
	fun (F, Cnt) ->
		receive
		    {Ref, Parent} ->
			Parent ! {Ref, Cnt}
		after 0 ->
			erlang:yield(),
			F(F, Cnt+1)
		end
	end,
    Pid = spawn_link(fun () -> Fun(Fun, 0) end),
    {Ref, Pid}.

schedcnt(stop, {Ref, Pid}) when is_reference(Ref), is_pid(Pid) ->
    Pid ! {Ref, self()},
    receive
	{Ref, Cnt} ->
	    Cnt
    end.

yield2(Config) when is_list(Config) ->
    Me = self(),
    Go = make_ref(),
    RedDiff = make_ref(),
    Done = make_ref(),
    P = spawn(fun () ->
		receive Go -> ok end,
		{reductions, R1} = process_info(self(), reductions),
		{ok, true} = call_yield(middle),
		true = call_yield(final),
		true = call_yield(),
		true = apply(erlang, yield, []),
		{reductions, R2} = process_info(self(), reductions),
		Me ! {RedDiff, R2 - R1},
		exit(Done)
	end),
    erlang:yield(),

    1 = erlang:trace(P, true, [running, procs, {tracer, self()}]),

    P ! Go,

    %% receive Go -> ok end,
    {trace, P, in, _} = next_tmsg(P),

    %% {ok, true} = call_yield(middle),
    {trace, P, out, _} = next_tmsg(P),
    {trace, P, in, _} = next_tmsg(P),

    %% true = call_yield(final),
    {trace, P, out, _} = next_tmsg(P),
    {trace, P, in, _} = next_tmsg(P),

    %% true = call_yield(),
    {trace, P, out, _} = next_tmsg(P),
    {trace, P, in, _} = next_tmsg(P),

    %% true = apply(erlang, yield, []),
    {trace, P, out, _} = next_tmsg(P),
    {trace, P, in, _} = next_tmsg(P),

    %% exit(Done)
    {trace, P, exit, Done} = next_tmsg(P),


    receive
	      {RedDiff, Reductions} when Reductions < 30, Reductions > 0 ->
		  io:format("Reductions = ~p~n", [Reductions]),
		  ok;
	      {RedDiff, Reductions} ->
		  ct:fail({unexpected_reduction_count, Reductions})
	  end,

    none = next_tmsg(P),

    ok.

next_tmsg(Pid) ->
    receive
	TMsg when is_tuple(TMsg),
		  element(1, TMsg) == trace,
		  element(2, TMsg) == Pid ->
	    TMsg
    after 100 ->
	    none
    end.

dist_unlink_ack_exit_leak(Config) when is_list(Config) ->
    %% Verification of nc reference counts when stopping node
    %% will find the bug if it exists...
    {ok, Peer, Node} = ?CT_PEER(),
    ParentFun =
        fun () ->
                %% Give parent some work to do when
                %% exiting to increase the likelyhood
                %% of the bug triggereing...
                T = ets:new(x,[]),
                ets:insert(T, lists:map(fun (I) ->
                                                {I,I}
                                        end,
                                        lists:seq(1,10000))),
                Chld = spawn_link(Node,
                                  fun () ->
                                          receive
                                          after infinity ->
                                                  ok
                                          end
                                  end),
                erlang:yield(),
                unlink(Chld),
                exit(bye)
        end,
    PMs = lists:map(fun (_) ->
                            spawn_monitor(ParentFun)
                    end, lists:seq(1, 10)),
    lists:foreach(fun ({P, M}) ->
                          receive
                              {'DOWN', M, process, P, bye} ->
                                  ok
                          end
                  end, PMs),
    stop_node(Peer, Node),
    ok.

%% Test that bad arguments to register/2 cause an exception.
bad_register(Config) when is_list(Config) ->
    Name = a_long_and_unused_name,

    {'EXIT',{badarg,_}} = (catch register({bad,name}, self())),
    fail_register(undefined, self()),
    fail_register([bad,name], self()),

    {Dead,Mref} = spawn_monitor(fun() -> true end),
    receive
	{'DOWN',Mref,process,Dead,_} -> ok
    end,
    fail_register(Name, Dead),
    fail_register(Name, make_ref()),
    fail_register(Name, []),
    fail_register(Name, {bad,process}),
    fail_register(Name, <<>>),
    ok.

fail_register(Name, Process) ->
    {'EXIT',{badarg,_}} = (catch register(Name, Process)),
    {'EXIT',{badarg,_}} = (catch Name ! anything_goes),
    ok.

garbage_collect(Config) when is_list(Config) ->
    Prio = process_flag(priority, high),
    true = erlang:garbage_collect(),
    
    TokLoopers = lists:map(fun (_) ->
		spawn_opt(fun tok_loop/0, [{priority, low}, link])
	end, lists:seq(1, 10)),

    lists:foreach(fun (Pid) ->
		Mon = erlang:monitor(process, Pid),
		DownBefore = receive
		    {'DOWN', Mon, _, _, _} ->
			true
		after 0 ->
			false
		end,
		GC = erlang:garbage_collect(Pid),
		DownAfter = receive
		    {'DOWN', Mon, _, _, _} ->
			true
		after 0 ->
			false
		end,
		true = erlang:demonitor(Mon),
		case {DownBefore, DownAfter} of
		    {true, _} -> false = GC;
		    {false, false} -> true = GC;
		    _ -> GC
		end
	end, processes()),

    lists:foreach(fun (Pid) ->
		unlink(Pid),
		exit(Pid, bang)
	end, TokLoopers),
    process_flag(priority, Prio),
    ok.

%% This used to cause the nofrag emulator to dump core
process_info_messages(Config) when is_list(Config) ->
    process_info_messages_test(),
    ok.

process_info_messages_loop(0) -> ok;
process_info_messages_loop(N) -> process_info_messages_loop(N-1).

process_info_messages_send_my_msgs_to(Rcvr) ->
    receive
	Msg ->
	    Rcvr ! Msg,
	    process_info_messages_send_my_msgs_to(Rcvr)
    after 0 ->
	    ok
    end.

process_info_messages_test() ->
    Go = make_ref(),
    Done = make_ref(),
    Rcvr = self(),
    Rcvr2 = spawn_link(fun () ->
		receive {Go, Rcvr} -> ok end,
		garbage_collect(),
		Rcvr ! {Done, self()}
	end),
    Sndrs = lists:map(
	fun (_) ->
		spawn_link(fun () ->
			    Rcvr ! {Go, self()},
			    receive {Go, Rcvr} -> ok end,
			    BigData = lists:seq(1, 1000),
			    Rcvr ! BigData,
			    Rcvr ! BigData,
			    Rcvr ! BigData,
			    Rcvr ! {Done, self()}
		    end)
	end, lists:seq(1, 10)),
    lists:foreach(fun (Sndr) -> receive {Go, Sndr} -> ok end end,
			Sndrs),
    garbage_collect(),
    erlang:yield(),
    lists:foreach(fun (Sndr) -> Sndr ! {Go, self()} end, Sndrs),
    process_info_messages_loop(100000000),
    Msgs = process_info(self(), messages),
    lists:foreach(fun (Sndr) -> receive {Done, Sndr} -> ok end end,
			Sndrs),
    garbage_collect(),
    Rcvr2 ! Msgs,
    process_info_messages_send_my_msgs_to(Rcvr2),
    Rcvr2 ! {Go, self()},
    garbage_collect(),
    receive {Done, Rcvr2} -> ok end,
    Msgs.

chk_badarg(Fun) ->
    try Fun(), exit(no_badarg) catch error:badarg -> ok end.

process_flag_badarg(Config) when is_list(Config) ->
    chk_badarg(fun () -> process_flag(gurka, banan) end),
    chk_badarg(fun () -> process_flag(trap_exit, gurka) end),
    chk_badarg(fun () -> process_flag(error_handler, 1) end),
    chk_badarg(fun () -> process_flag(fullsweep_after, gurka) end),
    chk_badarg(fun () -> process_flag(min_heap_size, gurka) end),
    chk_badarg(fun () -> process_flag(min_bin_vheap_size, gurka) end),
    chk_badarg(fun () -> process_flag(min_bin_vheap_size, -1) end),

    chk_badarg(fun () -> process_flag(max_heap_size, gurka) end),
    chk_badarg(fun () -> process_flag(max_heap_size, -1) end),
    chk_badarg(fun () ->
                       {_,Min} = process_info(self(), min_heap_size),
                       process_flag(max_heap_size, Min - 1)
               end),
    chk_badarg(fun () ->
                       {_,Min} = process_info(self(), min_heap_size),
                       process_flag(max_heap_size, #{size => Min - 1})
               end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{}) end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{ kill => true }) end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{ size => 233,
                                                        kill => gurka }) end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{ size => 233,
                                                        error_logger => gurka }) end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{ size => 233,
                                                        include_shared_binaries => gurka}) end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{ size => 233,
                                                        kill => true,
                                                        error_logger => gurka }) end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{ size => 1 bsl 64 }) end),

    chk_badarg(fun () -> process_flag(priority, 4711) end),
    chk_badarg(fun () -> process_flag(save_calls, hmmm) end),
    {P,Mref} = spawn_monitor(fun () -> receive "in vain" -> no end end),
    chk_badarg(fun () -> process_flag(P, save_calls, hmmm) end),
    chk_badarg(fun () -> process_flag(gurka, save_calls, hmmm) end),
    exit(P, die),
    chk_badarg(fun () -> process_flag(P, save_calls, 0) end),
    {'DOWN', Mref, process, P, die} = receive M -> M end,
    chk_badarg(fun () -> process_flag(P, save_calls, 0) end),
    ok.

-include_lib("stdlib/include/ms_transform.hrl").

otp_6237(Config) when is_list(Config) ->
    Slctrs = lists:map(fun (_) ->
		spawn_link(fun () ->
			    otp_6237_select_loop()
		    end)
	end,
	lists:seq(1,5)),
    lists:foreach(fun (_) -> otp_6237_test() end, lists:seq(1, 100)),
    lists:foreach(fun (S) -> unlink(S),exit(S, kill) end, Slctrs), 
    ok.
				
otp_6237_test() ->
    Parent = self(),
    Inited = make_ref(),
    Die = make_ref(),
    Pid = spawn_link(fun () ->
		register(otp_6237,self()),
		otp_6237 = ets:new(otp_6237,
		    [named_table,
			ordered_set]),
		ets:insert(otp_6237,
		    [{I,I}
			|| I <- lists:seq(1, 100)]),
		%% Inserting a lot of bif timers
		%% increase the possibility that
		%% the test will fail when the
		%% original cleanup order is used
		lists:foreach( fun (_) ->
			    erlang:send_after(1000000, self(), {a,b,c})
		    end, lists:seq(1,1000)),
		Parent ! Inited,
		receive Die -> bye end
	end),
    receive
	Inited -> ok
    end,
    Pid ! Die,
    otp_6237_whereis_loop().

otp_6237_whereis_loop() ->
    case whereis(otp_6237) of
	      undefined ->
		  otp_6237 = ets:new(otp_6237,
					   [named_table,ordered_set]),
		  ets:delete(otp_6237),
		  ok;
	      _ ->
		  otp_6237_whereis_loop()
	  end.
	     
otp_6237_select_loop() ->
    catch ets:select(otp_6237, ets:fun2ms(fun({K, does_not_exist}) -> K end)),
    otp_6237_select_loop().


-define(NoTestProcs, 10000).
-record(ptab_list_bif_info, {min_start_reds,
			     tab_chunks,
			     tab_chunks_size,
			     tab_indices_per_red,
			     free_term_proc_reds,
			     term_procs_per_red,
			     term_procs_max_reds,
			     conses_per_red,
			     debug_level}).

processes_large_tab(Config) when is_list(Config) ->
    sys_mem_cond_run(2048, fun () -> processes_large_tab_test() end).

processes_large_tab_test() ->
    enable_internal_state(),
    MaxDbgLvl = 20,
    MinProcTabSize = 2*(1 bsl 15),
    ProcTabSize0 = 1000000,
    ProcTabSize1 = case {erlang:system_info(schedulers_online),
	    erlang:system_info(logical_processors)} of
	{Schdlrs, Cpus} when is_integer(Cpus),
	Schdlrs =< Cpus ->
	    ProcTabSize0;
	_ ->
	    ProcTabSize0 div 4
    end,
    BT = erlang:system_info(build_type),
    ProcTabSize2 = case (BT =:= debug) or (BT =:= valgrind) of
	true -> ProcTabSize1 - 500000;
	false -> ProcTabSize1
    end,
    %% With high debug levels this test takes so long time that
    %% the connection times out; therefore, shrink the test on
    %% high debug levels.
    DbgLvl = case erts_debug:get_internal_state(processes_bif_info) of
		       #ptab_list_bif_info{debug_level = Lvl} when Lvl > MaxDbgLvl ->
			   20;
		       #ptab_list_bif_info{debug_level = Lvl} when Lvl < 0 ->
			   ct:fail({debug_level, Lvl});
		       #ptab_list_bif_info{debug_level = Lvl} ->
			   Lvl
		   end,
    ProcTabSize3 = ProcTabSize2 - (1300000 * DbgLvl div MaxDbgLvl),
    ProcTabSize = case ProcTabSize3 < MinProcTabSize of
			    true -> MinProcTabSize;
			    false -> ProcTabSize3
			end,
    {ok, Peer, LargeNode} = ?CT_PEER(["+P", integer_to_list(ProcTabSize)]),
    Res = rpc:call(LargeNode, ?MODULE, processes_bif_test, []),
    case rpc:call(LargeNode,
			erts_debug,
			get_internal_state,
			[processes_bif_info]) of
	      #ptab_list_bif_info{tab_chunks = Chunks} when is_integer(Chunks),
							    Chunks > 1 -> ok;
	      PBInfo -> ct:fail(PBInfo)
	  end,
    stop_node(Peer, LargeNode),
    chk_processes_bif_test_res(Res).

processes_default_tab(Config) when is_list(Config) ->
    sys_mem_cond_run(1024, fun () -> processes_default_tab_test() end).

processes_default_tab_test() ->
    {ok, Peer, DefaultNode} = ?CT_PEER(),
    Res = rpc:call(DefaultNode, ?MODULE, processes_bif_test, []),
    stop_node(Peer, DefaultNode),
    chk_processes_bif_test_res(Res).

processes_small_tab(Config) when is_list(Config) ->
    {ok, Peer, SmallNode} = ?CT_PEER(["+P","1024"]),
    Res    = rpc:call(SmallNode, ?MODULE, processes_bif_test, []),
    PBInfo = rpc:call(SmallNode, erts_debug, get_internal_state, [processes_bif_info]),
    stop_node(Peer, SmallNode),
    true = PBInfo#ptab_list_bif_info.tab_chunks < 10,
    chk_processes_bif_test_res(Res).

processes_this_tab(Config) when is_list(Config) ->
    Mem = case {erlang:system_info(build_type),
                erlang:system_info(allocator)} of
              {lcnt, {_, _Vsn, [sys_alloc], _Opts}} ->
                  %% When running +Mea min + lcnt we may need more memory
                  1024 * 4;
              _ ->
                  1024
          end,
    sys_mem_cond_run(Mem, fun () -> chk_processes_bif_test_res(processes_bif_test()) end).

chk_processes_bif_test_res(ok) -> ok;
chk_processes_bif_test_res({comment, _} = Comment) -> Comment;
chk_processes_bif_test_res(Failure) -> ct:fail(Failure).

print_processes_bif_info(#ptab_list_bif_info{min_start_reds = MinStartReds,
					     tab_chunks = TabChunks,
					     tab_chunks_size = TabChunksSize,
					     tab_indices_per_red = TabIndPerRed,
					     free_term_proc_reds = FreeTPReds,
					     term_procs_per_red = TPPerRed,
					     term_procs_max_reds = TPMaxReds,
					     conses_per_red = ConsesPerRed,
					     debug_level = DbgLvl}) ->
    io:format("processes/0 bif info on node ~p:~n"
	      "Min start reductions = ~p~n"
	      "Process table chunks = ~p~n"
	      "Process table chunks size = ~p~n"
	      "Process table indices per reduction = ~p~n"
	      "Reduction cost for free() on terminated process struct = ~p~n"
	      "Inspect terminated processes per reduction = ~p~n"
	      "Max reductions during inspection of terminated processes = ~p~n"
	      "Create cons-cells per reduction = ~p~n"
	      "Debug level = ~p~n",
	      [node(),
	       MinStartReds,
	       TabChunks,
	       TabChunksSize,
	       TabIndPerRed,
	       FreeTPReds,
	       TPPerRed,
	       TPMaxReds,
	       ConsesPerRed,
	       DbgLvl]).

processes_bif_cleaner() ->
    receive {'EXIT', _, _} -> ok end,
    processes_bif_cleaner().

spawn_initial_hangarounds(Cleaner) ->
    TabSz = erlang:system_info(process_limit),
    erts_debug:set_internal_state(next_pid,TabSz),
    spawn_initial_hangarounds(Cleaner,
			      TabSz,
			      TabSz*2,
			      0,
			      []).

processes_unexpected_result(CorrectProcs, Procs) ->
    ProcInfo = [registered_name,
		initial_call,
		current_function,
		status,
		priority],
    MissingProcs = CorrectProcs -- Procs,
    io:format("Missing processes: ~p",
	      [lists:map(fun (Pid) ->
				 [{pid, Pid}
				  | case process_info(Pid, ProcInfo) of
					undefined -> [];
					Res -> Res
				    end]
			 end,
			 MissingProcs)]),
    SuperfluousProcs = Procs -- CorrectProcs,
    io:format("Superfluous processes: ~p",
	      [lists:map(fun (Pid) ->
				 [{pid, Pid}
				  | case process_info(Pid, ProcInfo) of
					undefined -> [];
					Res -> Res
				    end]
			 end,
			 SuperfluousProcs)]),
    ct:fail(unexpected_result).

hangaround(Cleaner, Type) ->
    %% Type is only used to distinguish different processes from
    %% when doing process_info
    try link(Cleaner) catch error:Reason -> exit(Reason) end,
    receive after infinity -> ok end,
    exit(Type).

spawn_initial_hangarounds(_Cleaner, NP, Max, Len, HAs) when NP > Max ->
    {Len, HAs};
spawn_initial_hangarounds(Cleaner, NP, Max, Len, HAs) ->
    Skip = 30,
    wait_for_proc_slots(Skip+3),
    HA1 = spawn_opt(?MODULE, hangaround, [Cleaner, initial_hangaround],
		    [{priority, low}]),
    HA2 = spawn_opt(?MODULE, hangaround, [Cleaner, initial_hangaround],
		    [{priority, normal}]),
    HA3 = spawn_opt(?MODULE, hangaround, [Cleaner, initial_hangaround],
		    [{priority, high}]),
    spawn_drop(Skip),
    spawn_initial_hangarounds(Cleaner, NP+Skip, Max, Len+3, [HA1,HA2,HA3|HAs]).

wait_for_proc_slots(MinFreeSlots) ->
    case erlang:system_info(process_limit) - erlang:system_info(process_count) of
        FreeSlots when FreeSlots < MinFreeSlots ->
            receive after 10 -> ok end,
            wait_for_proc_slots(MinFreeSlots);
        _FreeSlots ->
            ok
    end.

spawn_drop(N) when N =< 0 ->
    ok;
spawn_drop(N) ->
    spawn(fun () -> ok end),
    spawn_drop(N-1).

do_processes(WantReds) ->
    erts_debug:set_internal_state(reds_left, WantReds),
    processes().

processes_bif_test() ->
    Tester = self(),
    enable_internal_state(),
    PBInfo = erts_debug:get_internal_state(processes_bif_info),
    print_processes_bif_info(PBInfo),
    WantReds = PBInfo#ptab_list_bif_info.min_start_reds + 10,
    WillTrap = case PBInfo of
	#ptab_list_bif_info{tab_chunks = Chunks} when Chunks < 10 ->
	    false; %% Skip for small tables
	#ptab_list_bif_info{tab_chunks = Chunks,
	    tab_chunks_size = ChunksSize,
	    tab_indices_per_red = IndiciesPerRed
	} ->
	    Chunks*ChunksSize >= IndiciesPerRed*WantReds
    end,
    Processes = fun () ->
	    erts_debug:set_internal_state(reds_left,WantReds),
	    processes()
    end,

    IterProcesses =
        fun () ->
                erts_debug:set_internal_state(reds_left, WantReds),
                iter_all_processes()
        end,

    ok = do_processes_bif_test(WantReds, WillTrap, Processes),
    ok = do_processes_bif_test(WantReds, false, IterProcesses()),

    case WillTrap of
	false ->
	    ok;
	true ->
	    %% Do it against with a process suspended while
	    %% in the processes/0 bif.
	    erlang:system_flag(multi_scheduling, block_normal),
	    Suspendee = spawn_link(fun () ->
						 Tester ! {suspend_me, self()},
						 Tester ! {self(),
							   done,
							   hd(Processes())},
						 receive
						 after infinity ->
							 ok
						 end
					 end),
	    receive {suspend_me, Suspendee} -> ok end,
	    erlang:suspend_process(Suspendee),
	    erlang:system_flag(multi_scheduling, unblock_normal),
	    
	    [{status,suspended},{current_function,{erlang,ptab_list_continue,2}}] =
		process_info(Suspendee, [status, current_function]),

	    ok = do_processes_bif_test(WantReds, WillTrap, Processes),
	    
	    erlang:resume_process(Suspendee),
	    receive {Suspendee, done, _} -> ok end,
	    unlink(Suspendee),
	    exit(Suspendee, bang)
    end,
    case get(processes_bif_testcase_comment) of
	undefined -> ok;
	Comment -> {comment, Comment}
    end.

iter_all_processes() ->
    Iter = erlang:processes_iterator(),
    iter_all_processes(Iter).

iter_all_processes(Iter0) ->
    case erlang:processes_next(Iter0) of
        {Pid, Iter} ->
            [Pid|iter_all_processes(Iter)];
        none ->
            none
    end.

do_processes_bif_test(WantReds, DieTest, Processes) ->
    Tester = self(),
    SpawnProcesses = fun (Prio) ->
	    spawn_opt(?MODULE, do_processes, [WantReds], [link, {priority, Prio}])
    end,
    Cleaner = spawn_link(fun () ->
		process_flag(trap_exit, true),
		Tester ! {cleaner_alive, self()},
		processes_bif_cleaner()
	end),
    receive {cleaner_alive, Cleaner} -> ok end,
    try
	DoIt = make_ref(),
	GetGoing = make_ref(),
	{NoTestProcs, TestProcs} = spawn_initial_hangarounds(Cleaner),
	io:format("Testing with ~p processes~n", [NoTestProcs]),
	SpawnHangAround = fun () ->
		spawn(?MODULE, hangaround, [Cleaner, new_hangaround])
	end,
	Killer = spawn_opt(fun () ->
		    Splt = NoTestProcs div 10,
		    {TP1, TP23} = lists:split(Splt, TestProcs),
		    {TP2, TP3} = lists:split(Splt, TP23),
		    erlang:system_flag(multi_scheduling, block_normal),
		    Tester ! DoIt,
		    receive GetGoing -> ok end,
		    erlang:system_flag(multi_scheduling, unblock_normal),
		    SpawnProcesses(high),
		    lists:foreach( fun (P) ->
				SpawnHangAround(),
				exit(P, bang)
			end, TP1),
		    SpawnProcesses(high),
		    erlang:yield(),
		    lists:foreach( fun (P) ->
				SpawnHangAround(),
				exit(P, bang)
			end, TP2),
		    SpawnProcesses(high),
		    lists:foreach(
			fun (P) ->
				SpawnHangAround(),
				exit(P, bang)
			end, TP3)
	    end, [{priority, high}, link]),
	receive DoIt -> ok end,
	process_flag(priority, low),
	SpawnProcesses(low),
	erlang:yield(),
	process_flag(priority, normal),
	CorrectProcs0 = erts_debug:get_internal_state(processes),
	Killer ! GetGoing,
	erts_debug:set_internal_state(reds_left, WantReds),
	Procs0 = processes(),
	Procs = lists:sort(Procs0),
	CorrectProcs = lists:sort(CorrectProcs0),
	LengthCorrectProcs = length(CorrectProcs),
	io:format("~p = length(CorrectProcs)~n", [LengthCorrectProcs]),
	true = LengthCorrectProcs > NoTestProcs,
	case CorrectProcs =:= Procs of
	    true ->
		ok;
	    false ->
		processes_unexpected_result(CorrectProcs, Procs)
	end,
	unlink(Killer),
	exit(Killer, bang)
    after
	unlink(Cleaner),
        exit(Cleaner, kill),
        %% Wait for the system to recover to a normal state...
	wait_until_system_recover()
    end,
    do_processes_bif_die_test(DieTest, Processes),
    ok.


do_processes_bif_die_test(false, _Processes) ->
    io:format("Skipping test killing process executing processes/0~n",[]),
    ok;
do_processes_bif_die_test(true, Processes) ->
    do_processes_bif_die_test(5, Processes);
do_processes_bif_die_test(N, Processes) ->
    io:format("Doing test killing process executing processes/0~n",[]),
    try
	Tester = self(),
	Oooh_Nooooooo = make_ref(),
	{_, DieWhileDoingMon} = erlang:spawn_monitor( fun () ->
		    Victim = self(),
		    spawn_opt(
			fun () ->
				exit(Victim, got_him)
			end,
			[link, {priority, max}]),
		    Tester ! {Oooh_Nooooooo,
			hd(Processes())},
		    exit(ohhhh_nooooo)
	    end),
	receive
	    {'DOWN', DieWhileDoingMon, _, _, Reason} ->
		case Reason of
		    got_him -> ok;
		    _ -> throw({kill_in_trap, Reason})
		end
	end,
	receive
	    {Oooh_Nooooooo, _} ->
		throw({kill_in_trap, 'Oooh_Nooooooo'})
	after 0 ->
		ok
	end,
	PrcsCllrsSeqLen = 2*erlang:system_info(schedulers_online),
	PrcsCllrsSeq = lists:seq(1, PrcsCllrsSeqLen),
	ProcsCallers = lists:map( fun (_) ->
		    spawn_link(
			fun () ->
				Tester ! hd(Processes())
			end)
	    end, PrcsCllrsSeq),
	erlang:yield(),
	{ProcsCallers1, ProcsCallers2} = lists:split(PrcsCllrsSeqLen div 2,
						     ProcsCallers),
	process_flag(priority, high),
	lists:foreach(
		fun (P) ->
			unlink(P),
			exit(P, bang)
		end,
		lists:reverse(ProcsCallers2) ++ ProcsCallers1),
	process_flag(priority, normal),
	ok
    catch
	throw:{kill_in_trap, R} when N > 0 ->
	    io:format("Failed to kill in trap: ~p~n", [R]),
	    io:format("Trying again~n", []),
	    do_processes_bif_die_test(N-1, Processes)
    end.
	    

wait_until_system_recover() ->
    %% If system hasn't recovered after 10 seconds we give up
    Tmr = erlang:start_timer(10000, self(), no_more_wait),
    wait_until_system_recover(Tmr).

wait_until_system_recover(Tmr) ->
    try
	lists:foreach(fun (P) when P == self() ->
			      ok;
			  (P) ->
			      case process_info(P, initial_call) of
				  {initial_call,{?MODULE, _, _}} ->
				      throw(wait);
				  {initial_call,{_, _, _}} ->
				      ok;
				  undefined ->
				      ok
			      end
		      end,
		      processes())
    catch
	throw:wait ->
	    receive
		{timeout, Tmr, _} ->
		    Comment = "WARNING: Test processes still hanging around!",
		    io:format("~s~n", [Comment]),
		    put(processes_bif_testcase_comment, Comment),
		    lists:foreach(
		      fun (P) when P == self() ->
			      ok;
			  (P) ->
			      case process_info(P, initial_call) of
				  {initial_call,{?MODULE, _, _} = MFA} ->
				      io:format("~p ~p~n", [P, MFA]);
				  {initial_call,{_, _, _}} ->
				      ok;
				  undefined ->
				      ok
			      end
		      end,
		      processes())
	    after 100 ->
		    wait_until_system_recover(Tmr)
	    end
    end,
    erlang:cancel_timer(Tmr),
    receive {timeout, Tmr, _} -> ok after 0 -> ok end,
    ok.

processes_last_call_trap(Config) when is_list(Config) ->
    enable_internal_state(),
    Processes = fun () -> processes() end,
    PBInfo = erts_debug:get_internal_state(processes_bif_info),
    print_processes_bif_info(PBInfo),
    WantReds = case PBInfo#ptab_list_bif_info.min_start_reds of
	R when R > 10 -> R - 1;
	_R -> 9
    end,
    lists:foreach(fun (_) ->
		erts_debug:set_internal_state(reds_left,
		    WantReds),
		Processes(),
		erts_debug:set_internal_state(reds_left,
		    WantReds),
		my_processes()
	end,
	lists:seq(1,100)).
    
my_processes() ->
    processes().

processes_apply_trap(Config) when is_list(Config) ->
    enable_internal_state(),
    PBInfo = erts_debug:get_internal_state(processes_bif_info),
    print_processes_bif_info(PBInfo),
    WantReds = case PBInfo#ptab_list_bif_info.min_start_reds of
	R when R > 10 -> R - 1;
	_R -> 9
    end,
    lists:foreach(fun (_) ->
		erts_debug:set_internal_state(reds_left,
		    WantReds),
		apply(erlang, processes, [])
	end, lists:seq(1,100)).

processes_gc_trap(Config) when is_list(Config) ->
    Tester = self(),
    enable_internal_state(),
    PBInfo = erts_debug:get_internal_state(processes_bif_info),
    print_processes_bif_info(PBInfo),
    WantReds = PBInfo#ptab_list_bif_info.min_start_reds + 10,
    Processes = fun () ->
	    erts_debug:set_internal_state(reds_left,WantReds),
	    processes()
    end,

    erlang:system_flag(multi_scheduling, block_normal),
    Suspendee = spawn_link(fun () ->
					 Tester ! {suspend_me, self()},
					 Tester ! {self(),
						   done,
						   hd(Processes())},
					 receive after infinity -> ok end
				 end),
    receive {suspend_me, Suspendee} -> ok end,
    erlang:suspend_process(Suspendee),
    erlang:system_flag(multi_scheduling, unblock_normal),
	    
    [{status,suspended}, {current_function,{erlang,ptab_list_continue,2}}]
	= process_info(Suspendee, [status, current_function]),

    erlang:garbage_collect(Suspendee),
    erlang:garbage_collect(Suspendee),
	    
    erlang:resume_process(Suspendee),
    receive {Suspendee, done, _} -> ok end,
    erlang:garbage_collect(Suspendee),
    erlang:garbage_collect(Suspendee),

    unlink(Suspendee),
    exit(Suspendee, bang),
    ok.

process_flag_fullsweep_after(Config) when is_list(Config) ->
    {fullsweep_after, OldFSA} = process_info(self(), fullsweep_after),
    OldFSA = process_flag(fullsweep_after, 12345),
    {fullsweep_after, 12345} = process_info(self(), fullsweep_after),
    12345 = process_flag(fullsweep_after, 0),
    {fullsweep_after, 0} = process_info(self(), fullsweep_after),
    0 = process_flag(fullsweep_after, OldFSA),
    ok.

process_flag_heap_size(Config) when is_list(Config) ->
    HSize  = 2586,   % must be gc fib+ number
    VHSize = 318187, % must be gc fib+ number
    OldHmin = erlang:process_flag(min_heap_size, HSize),
    {min_heap_size, HSize} = erlang:process_info(self(), min_heap_size),
    OldVHmin = erlang:process_flag(min_bin_vheap_size, VHSize),
    {min_bin_vheap_size, VHSize} = erlang:process_info(self(), min_bin_vheap_size),
    HSize = erlang:process_flag(min_heap_size, OldHmin),
    VHSize = erlang:process_flag(min_bin_vheap_size, OldVHmin),
    ok.

%% test that max_heap_size is correctly handled when passed via command line
command_line_max_heap_size(Config) when is_list(Config) ->
    %% test maximum heap size
    HMax = case erlang:system_info(wordsize) of
               8 -> (1 bsl 59) - 1;
               4 -> (1 bsl 27) - 1
           end,
    {ok, Peer, Node} = ?CT_PEER(["+hmax", integer_to_list(HMax)]),
    Pid = erlang:spawn(Node, fun () -> receive after infinity -> ok end end),
    {max_heap_size, #{size := HMax}} = rpc:call(Node, erlang, process_info, [Pid, max_heap_size]),
    peer:stop(Peer).

spawn_opt_heap_size(Config) when is_list(Config) ->
    HSize  = 987,   % must be gc fib+ number
    VHSize = 46422, % must be gc fib+ number
    Pid  = spawn_opt(fun () -> receive stop -> ok end end,
	[{min_heap_size, HSize},{ min_bin_vheap_size, VHSize}]),
    {min_heap_size, HSize} = process_info(Pid, min_heap_size),
    {min_bin_vheap_size, VHSize} = process_info(Pid, min_bin_vheap_size),
    Pid ! stop,
    ok.

spawn_opt_max_heap_size(_Config) ->

    error_logger:add_report_handler(?MODULE, self()),

    %% flush any prior messages in error_logger
    Pid = spawn(fun() -> ok = nok end),
    receive
        {error, _, {emulator, _, [Pid|_]}} ->
            flush()
    end,

    spawn_opt_max_heap_size_do(fun oom_fun/1),

    io:format("Repeat tests with refc binaries\n",[]),

    spawn_opt_max_heap_size_do(fun oom_bin_fun/1),

    error_logger:delete_report_handler(?MODULE),
    ok.

spawn_opt_max_heap_size_do(OomFun) ->
    Max = 2024,
    %% Test that numerical limit works
    max_heap_size_test(Max, Max, true, true, OomFun),

    %% Test that map limit works
    max_heap_size_test(#{ size => Max }, Max, true, true, OomFun),

    %% Test that no kill is sent
    max_heap_size_test(#{ size => Max, kill => false }, Max, false, true, OomFun),

    %% Test that no error_logger report is sent
    max_heap_size_test(#{ size => Max, error_logger => false }, Max, true, false, OomFun),

    %% Test that system_flag works
    erlang:system_flag(max_heap_size, OomFun(#{ size => 0, kill => false,
                                                error_logger => true})),
    max_heap_size_test(#{ size => Max }, Max, false, true, OomFun),
    max_heap_size_test(#{ size => Max, kill => true }, Max, true, true, OomFun),

    erlang:system_flag(max_heap_size, OomFun(#{ size => 0, kill => true,
                                                error_logger => false})),
    max_heap_size_test(#{ size => Max }, Max, true, false, OomFun),
    max_heap_size_test(#{ size => Max, error_logger => true }, Max, true, true, OomFun),

    erlang:system_flag(max_heap_size, OomFun(#{ size => 1 bsl 16, kill => true,
                                                error_logger => true})),
    max_heap_size_test(#{ }, 1 bsl 16, true, true, OomFun),

    erlang:system_flag(max_heap_size, #{ size => 0, kill => true,
                                         error_logger => true}),

    %% Test that ordinary case works as expected again
    max_heap_size_test(Max, Max, true, true, OomFun),
    ok.


mhs_spawn_opt(Option) when map_get(size, Option) > 0;
                           is_integer(Option) ->
    [{max_heap_size, Option}];
mhs_spawn_opt(_) ->
    [].

max_heap_size_test(Option, Size, Kill, ErrorLogger, OomFun) ->
    SpOpt = mhs_spawn_opt(OomFun(Option)),
    Pid = spawn_opt(fun()-> OomFun(run) end, SpOpt),
    {max_heap_size, MHSz} = erlang:process_info(Pid, max_heap_size),
    ct:log("Default: ~p~nOption: ~p~nProc: ~p~nSize = ~p~nSpOpt = ~p~n",
           [erlang:system_info(max_heap_size), Option, MHSz, Size, SpOpt]),

    #{ size := Size} = MHSz,

    Ref = erlang:monitor(process, Pid),
    if Kill ->
            receive
                {'DOWN', Ref, process, Pid, Reason} ->
                    killed = Reason
            end;
       true ->
            ok
    end,
    if ErrorLogger ->
            receive
                %% There must be at least one error message.
                {error, _, {emulator, _, [Pid|_]}} ->
                    ok
            end;
       true ->
            ok
    end,
    if not Kill ->
            exit(Pid, die),
            receive
                {'DOWN', Ref, process, Pid, die} ->
                    ok
            end,
            %% If the process was not killed, the limit may have
            %% been reached more than once and there may be
            %% more {error, ...} messages left.
            receive_error_messages(Pid);
       true ->
            ok
    end,

    %% Make sure that there are no unexpected messages.
    receive_unexpected().

oom_fun(Max) when is_integer(Max) -> Max;
oom_fun(Map) when is_map(Map)-> Map;
oom_fun(run) ->
    io:format("oom_fun() started\n",[]),
    oom_run_fun([], 100).

oom_run_fun(Acc0, 0) ->
    done;
oom_run_fun(Acc0, N) ->
    %% This is tail-recursive since the compiler is smart enough to figure
    %% out that a body-recursive variant never returns, and loops forever
    %% without keeping the list alive.
    timer:sleep(5),
    oom_run_fun([lists:seq(1, 1000) | Acc0], N-1).

oom_bin_fun(Max) when is_integer(Max) -> oom_bin_fun(#{size => Max});
oom_bin_fun(Map) when is_map(Map) -> Map#{include_shared_binaries => true};
oom_bin_fun(run) ->
    oom_bin_run_fun([], 10).

oom_bin_run_fun(Acc0, 0) ->
    done;
oom_bin_run_fun(Acc0, N) ->
    timer:sleep(5),
    oom_bin_run_fun([build_refc_bin(160, <<>>) | Acc0], N-1).

build_refc_bin(0, Acc) ->
    Acc;
build_refc_bin(N, Acc) ->
    build_refc_bin(N-1, <<Acc/binary, 0:(1000*8)>>).


receive_error_messages(Pid) ->
    receive
        {error, _, {emulator, _, [Pid|_]}} ->
            receive_error_messages(Pid)
    after 1000 ->
            ok
    end.

receive_unexpected() ->
    receive
        {info_report, _, _} ->
            %% May be an alarm message from os_mon. Ignore.
            receive_unexpected();
        M ->
            ct:fail({unexpected_message, M})
    after 10 ->
            ok
    end.

flush() ->
    receive
        _M -> flush()
    after 0 ->
            ok
    end.

%% error_logger report handler proxy
init(Pid) ->
    {ok, Pid}.

handle_event(Event, Pid) ->
    Pid ! Event,
    {ok, Pid}.

huge_arglist_child(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9,
                   A10, A11, A12, A13, A14, A15, A16, A17, A18, A19,
                   A20, A21, A22, A23, A24, A25, A26, A27, A28, A29,
                   A30, A31, A32, A33, A34, A35, A36, A37, A38, A39,
                   A40, A41, A42, A43, A44, A45, A46, A47, A48, A49,
                   A50, A51, A52, A53, A54, A55, A56, A57, A58, A59,
                   A60, A61, A62, A63, A64, A65, A66, A67, A68, A69,
                   A70, A71, A72, A73, A74, A75, A76, A77, A78, A79,
                   A80, A81, A82, A83, A84, A85, A86, A87, A88, A89,
                   A90, A91, A92, A93, A94, A95, A96, A97, A98, A99,
                   A100, A101, A102, A103, A104, A105, A106, A107, A108, A109,
                   A110, A111, A112, A113, A114, A115, A116, A117, A118, A119,
                   A120, A121, A122, A123, A124, A125, A126, A127, A128, A129,
                   A130, A131, A132, A133, A134, A135, A136, A137, A138, A139,
                   A140, A141, A142, A143, A144, A145, A146, A147, A148, A149,
                   A150, A151, A152, A153, A154, A155, A156, A157, A158, A159,
                   A160, A161, A162, A163, A164, A165, A166, A167, A168, A169,
                   A170, A171, A172, A173, A174, A175, A176, A177, A178, A179,
                   A180, A181, A182, A183, A184, A185, A186, A187, A188, A189,
                   A190, A191, A192, A193, A194, A195, A196, A197, A198, A199,
                   A200, A201, A202, A203, A204, A205, A206, A207, A208, A209,
                   A210, A211, A212, A213, A214, A215, A216, A217, A218, A219,
                   A220, A221, A222, A223, A224, A225, A226, A227, A228, A229,
                   A230, A231, A232, A233, A234, A235, A236, A237, A238, A239,
                   A240, A241, A242, A243, A244, A245, A246, A247, A248, A249,
                   A250, A251, A252, A253, A254) ->
    receive go -> ok end,
    exit([A0, A1, A2, A3, A4, A5, A6, A7, A8, A9,
          A10, A11, A12, A13, A14, A15, A16, A17, A18, A19,
          A20, A21, A22, A23, A24, A25, A26, A27, A28, A29,
          A30, A31, A32, A33, A34, A35, A36, A37, A38, A39,
          A40, A41, A42, A43, A44, A45, A46, A47, A48, A49,
          A50, A51, A52, A53, A54, A55, A56, A57, A58, A59,
          A60, A61, A62, A63, A64, A65, A66, A67, A68, A69,
          A70, A71, A72, A73, A74, A75, A76, A77, A78, A79,
          A80, A81, A82, A83, A84, A85, A86, A87, A88, A89,
          A90, A91, A92, A93, A94, A95, A96, A97, A98, A99,
          A100, A101, A102, A103, A104, A105, A106, A107, A108, A109,
          A110, A111, A112, A113, A114, A115, A116, A117, A118, A119,
          A120, A121, A122, A123, A124, A125, A126, A127, A128, A129,
          A130, A131, A132, A133, A134, A135, A136, A137, A138, A139,
          A140, A141, A142, A143, A144, A145, A146, A147, A148, A149,
          A150, A151, A152, A153, A154, A155, A156, A157, A158, A159,
          A160, A161, A162, A163, A164, A165, A166, A167, A168, A169,
          A170, A171, A172, A173, A174, A175, A176, A177, A178, A179,
          A180, A181, A182, A183, A184, A185, A186, A187, A188, A189,
          A190, A191, A192, A193, A194, A195, A196, A197, A198, A199,
          A200, A201, A202, A203, A204, A205, A206, A207, A208, A209,
          A210, A211, A212, A213, A214, A215, A216, A217, A218, A219,
          A220, A221, A222, A223, A224, A225, A226, A227, A228, A229,
          A230, A231, A232, A233, A234, A235, A236, A237, A238, A239,
          A240, A241, A242, A243, A244, A245, A246, A247, A248, A249,
          A250, A251, A252, A253, A254]).

spawn_huge_arglist(Config) when is_list(Config) ->
    %% Huge in two different ways; encoded size and
    %% length...
    ArgListHead = [make_ref(),
                   lists:duplicate(1000000, $a),
                   <<1:8388608>>,
                   processes(),
                   erlang:ports(),
                   {hej, hopp},
                   <<17:8388608>>,
                   lists:duplicate(3000000, $x),
                   #{ a => 1, b => 2, c => 3, d => 4, e => 5}],
    ArgList = ArgListHead ++ lists:seq(1, 255 - length(ArgListHead)),

    io:format("size(term_to_binary(ArgList)) = ~p~n",
              [size(term_to_binary(ArgList))]),

    io:format("Testing spawn with huge argument list on local node...~n", []),
    spawn_huge_arglist_test(true, node(), ArgList),
    io:format("Testing spawn with huge argument list on local node with Node...~n", []),
    spawn_huge_arglist_test(false, node(), ArgList),
    {ok, Peer, Node} = ?CT_PEER(),
    _ = rpc:call(Node, ?MODULE, module_info, []),
    io:format("Testing spawn with huge argument list on remote node ~p...~n", [Node]),
    spawn_huge_arglist_test(false, Node, ArgList),
    stop_node(Peer, Node),
    ok.

spawn_huge_arglist_test(Local, Node, ArgList) ->

    R1 = case Local of
             true ->
                 spawn_request(?MODULE, huge_arglist_child, ArgList, [monitor]);
             false ->
                 spawn_request(Node, ?MODULE, huge_arglist_child, ArgList, [monitor])
         end,
    receive
        {spawn_reply, R1, ok, Pid1} ->
            Pid1 ! go,
            receive
                {'DOWN', R1, process, Pid1, Reason1} ->
                    ArgList = Reason1
            end
    end,

    {Pid2, R2} = case Local of
                     true ->
                         spawn_monitor(?MODULE, huge_arglist_child, ArgList);
                     false ->
                         spawn_monitor(Node, ?MODULE, huge_arglist_child, ArgList)
                 end,
    Node = node(Pid2),
    Pid2 ! go,
    receive
        {'DOWN', R2, process, Pid2, Reason2} ->
            ArgList = Reason2
    end,
    
    {Pid3, R3} = case Local of
                     true ->
                         spawn_opt(?MODULE, huge_arglist_child, ArgList, [monitor]);
                     false ->
                         spawn_opt(Node, ?MODULE, huge_arglist_child, ArgList, [monitor])
                 end,
    Node = node(Pid3),
    Pid3 ! go,
    receive
        {'DOWN', R3, process, Pid3, Reason3} ->
            ArgList = Reason3
    end,

    OldTA = process_flag(trap_exit, true),
    Pid4 = case Local of
               true ->
                   spawn_link(?MODULE, huge_arglist_child, ArgList);
               false ->
                   spawn_link(Node, ?MODULE, huge_arglist_child, ArgList)
           end,
    Node = node(Pid4),
    Pid4 ! go,
    receive
        {'EXIT', Pid4, Reason4} ->
            ArgList = Reason4
    end,

    true = process_flag(trap_exit, OldTA),

    Pid5 = case Local of
               true ->
                   spawn(?MODULE, huge_arglist_child, ArgList);
               false ->
                   spawn(Node, ?MODULE, huge_arglist_child, ArgList)
           end,
    Node = node(Pid5),
    R5 = erlang:monitor(process, Pid5),
    Pid5 ! go,
    receive
        {'DOWN', R5, process, Pid5, Reason5} ->
            ArgList = Reason5
    end,
    ok.

spawn_request_bif(Config) when is_list(Config) ->
    io:format("Testing spawn_request() on local node...~n", []),
    spawn_request_bif_test(true, node()),
    io:format("Testing spawn_request() on local node with Node...~n", []),
    spawn_request_bif_test(false, node()),
    {ok, Peer, Node} = ?CT_PEER(),
    io:format("Testing spawn_request() on remote node ~p...~n", [Node]),
    spawn_request_bif_test(false, Node),
    stop_node(Peer, Node),
    ok.
                       
spawn_request_bif_test(Local, Node) ->

    Me = self(),

    process_flag(trap_exit, true),

    T1 = {test, 1},
    F1 = fun () -> exit({exit, T1}) end,
    R1 = if Local ->
                 spawn_request(F1, [{reply_tag, T1}, monitor, link]);
            true ->
                 spawn_request(Node, F1, [{reply_tag, T1}, monitor, link])
         end,
    receive
        {T1, R1, ok, P1} ->
            receive
                {'DOWN', R1, process, P1, {exit, T1}} ->
                    ok
            end,
            receive
                {'EXIT', P1, {exit, T1}} ->
                    ok
            end
    end,

    R1b = if Local ->
                 spawn_request(F1, [monitor, link]);
            true ->
                 spawn_request(Node, F1, [monitor, link])
         end,
    receive
        {spawn_reply, R1b, ok, P1b} ->
            receive
                {'DOWN', R1b, process, P1b, {exit, T1}} ->
                    ok
            end,
            receive
                {'EXIT', P1b, {exit, T1}} ->
                    ok
            end
    end,

    Ref1c = make_ref(),
    F1c = fun () -> Me ! Ref1c end,
    R1c = if Local ->
                  spawn_request(F1c);
             true ->
                  spawn_request(Node, F1c)
            end,
    receive
        {spawn_reply, R1c, ok, _P1c} ->
            receive Ref1c -> ok end
    end,

    R1e = if Local ->
                 spawn_request(F1, [monitors, links, {reply_tag, T1}]);
            true ->
                 spawn_request(Node, F1, [monitors, links, {reply_tag, T1}])
         end,
    receive
        {T1, R1e, error, BadOpt1} ->
            badopt = BadOpt1,
            ok
    end,
    ok = try
             BadF = fun (X) -> exit({X,T1}) end,
             if Local ->
                     spawn_request(BadF, [monitor, {reply_tag, T1}, link]);
                true ->
                     spawn_request(Node, BadF, [monitor, {reply_tag, T1}, link])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             spawn_request(<<"node">>, F1, [monitor, link], T1),
             nok
         catch
             error:badarg -> ok
         end,

    T2 = {test, 2},
    M2 = erlang,
    F2 = exit,
    Reason2 = {exit, T2},
    Args2 = [Reason2],
    R2 = if Local ->
                 spawn_request(M2, F2, Args2, [monitor, link, {reply_tag, T2}]);
            true ->
                 spawn_request(Node, M2, F2, Args2, [monitor, link, {reply_tag, T2}])
            end,
    receive
        {T2, R2, ok, P2} ->
            receive
                {'DOWN', R2, process, P2, Reason2} ->
                    ok
            end,
            receive
                {'EXIT', P2, Reason2} ->
                    ok
            end
    end,

    R2b = if Local ->
                 spawn_request(M2, F2, Args2, [monitor, link]);
            true ->
                 spawn_request(Node, M2, F2, Args2, [monitor, link])
            end,
    receive
        {spawn_reply, R2b, ok, P2b} ->
            receive
                {'DOWN', R2b, process, P2b, Reason2} ->
                    ok
            end,
            receive
                {'EXIT', P2b, Reason2} ->
                    ok
            end
    end,

    Ref2c = make_ref(),
    R2c = if Local ->
                  spawn_request(erlang, send, [Me, Ref2c]);
             true ->
                  spawn_request(Node, erlang, send, [Me, Ref2c])
            end,
    receive
        {spawn_reply, R2c, ok, _P2c} ->
            receive Ref2c -> ok end
    end,

    R2e = if Local ->
                 spawn_request(M2, F2, Args2, [monitors, {reply_tag, T2}, links]);
            true ->
                 spawn_request(Node, M2, F2, Args2, [monitors, {reply_tag, T2}, links])
         end,
    receive
        {T2, R2e, error, BadOpt2} ->
            badopt = BadOpt2,
            ok
    end,

    R2eb = if Local ->
                 spawn_request(M2, F2, Args2, [monitors, links]);
            true ->
                 spawn_request(Node, M2, F2, Args2, [monitors, links])
         end,
    receive
        {spawn_reply, R2eb, error, BadOpt2b} ->
            badopt = BadOpt2b,
            ok
    end,

    ok = try
             if Local ->
                     spawn_request(M2, F2, [Args2|oops], [monitor, link, {reply_tag, T2}]);
                true ->
                     spawn_request(Node, M2, F2, [Args2|oops], [monitor, link, {reply_tag, T2}])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(M2, F2, [Args2|oops], [monitor, {reply_tag, blupp}, link]);
                true ->
                     spawn_request(Node, M2, F2, [Args2|oops], [monitor, {reply_tag, blupp}, link])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(M2, F2, [Args2|oops]);
                true ->
                     spawn_request(Node, M2, F2, [Args2|oops])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(M2, <<"exit">>, Args2, [monitor, {reply_tag, T2}, link]);
                true ->
                     spawn_request(Node, M2, <<"exit">>, Args2, [monitor, {reply_tag, T2}, link])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(M2, <<"exit">>, Args2, [monitor, link]);
                true ->
                     spawn_request(Node, M2, <<"exit">>, Args2, [monitor, link])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(M2, <<"exit">>, Args2);
                true ->
                     spawn_request(Node, M2, <<"exit">>, Args2)
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(<<"erlang">>, F2, Args2, [{reply_tag, T2}, monitor, link]);
                true ->
                     spawn_request(Node, <<"erlang">>, F2, Args2, [{reply_tag, T2}, monitor, link])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(<<"erlang">>, F2, Args2, [monitor, link]);
                true ->
                     spawn_request(Node, <<"erlang">>, F2, Args2, [monitor, link])
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             if Local ->
                     spawn_request(<<"erlang">>, F2, Args2);
                true ->
                     spawn_request(Node, <<"erlang">>, F2, Args2)
             end,
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             spawn_request(<<"node">>, M2, F2, Args2, [{reply_tag, T2}, monitor, link]),
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             spawn_request(<<"node">>, M2, F2, Args2, [monitor, link]),
             nok
         catch
             error:badarg -> ok
         end,
    ok = try
             spawn_request(<<"node">>, M2, F2, Args2),
             nok
         catch
             error:badarg -> ok
         end,
    ok.


spawn_request_monitor_demonitor(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    BlockFun = fun () ->
                       erts_debug:set_internal_state(available_internal_state, true),
                       erts_debug:set_internal_state(block, 1000),
                       ok
               end,

    %% Block receiver node...
    spawn_request(Node, BlockFun, [{priority,max}, link]),
    receive after 100 -> ok end,

    erlang:yield(),
    R = spawn_request(Node, timer, sleep, [10000], [monitor]),
    %% Should not be possible to demonitor
    %% before operation has succeeded...
    {monitors, []} = process_info(self(), monitors),
    false = erlang:demonitor(R, [info]), %% Should be ignored by VM...
    receive
        {spawn_reply, R, ok, P} ->
            {monitors, [{process,P}]} = process_info(self(), monitors),
            true = erlang:demonitor(R, [info]),
            {monitors, []} = process_info(self(), monitors),
            exit(P, kill)
    end,
    stop_node(Peer, Node),
    ok.

spawn_request_monitor_child_exit(Config) when is_list(Config) ->
    %% Early child exit...
    Tag = {a, tag},
    R1 = spawn_request(nonexisting_module, nonexisting_function, [], [monitor, {reply_tag, Tag}]),
    receive
        {Tag, R1, ok, P1} ->
            receive
                {'DOWN', R1, process, P1, Reason1} ->
                    {undef, _} = Reason1
            end
    end,
    {ok, Peer, Node} = ?CT_PEER(),
    R2 = spawn_request(Node, nonexisting_module, nonexisting_function, [], [{reply_tag, Tag}, monitor]),
    receive
        {Tag, R2, ok, P2} ->
            receive
                {'DOWN', R2, process, P2, Reason2} ->
                    {undef, _} = Reason2
            end
    end,
    stop_node(Peer, Node),
    ok.

spawn_request_link_child_exit(Config) when is_list(Config) ->
    %% Early child exit...
    process_flag(trap_exit, true),
    Tag = {a, tag},
    R1 = spawn_request(nonexisting_module, nonexisting_function, [], [{reply_tag, Tag}, link]),
    receive
        {Tag, R1, ok, P1} ->
            receive
                {'EXIT', P1, Reason1} ->
                    {undef, _} = Reason1
            end
    end,
    {ok, Peer, Node} = ?CT_PEER(),
    R2 = spawn_request(Node, nonexisting_module, nonexisting_function, [], [link, {reply_tag, Tag}]),
    receive
        {Tag, R2, ok, P2} ->
            receive
                {'EXIT', P2, Reason2} ->
                    {undef, _} = Reason2
            end
    end,
    stop_node(Peer, Node),
    ok.

spawn_request_link_parent_exit(Config) when is_list(Config) ->
    C1 = spawn_request_link_parent_exit_test(node(), false),
    {ok, Peer, Node} = ?CT_PEER(),
    C2 = spawn_request_link_parent_exit_test(Node, false),
    stop_node(Peer, Node),
    {comment, C1 ++ " " ++ C2}.

spawn_request_link_parent_exit_compound_reason(Config) when is_list(Config) ->
    C1 = spawn_request_link_parent_exit_test(node(), true),
    {ok, Peer, Node} = ?CT_PEER(),
    C2 = spawn_request_link_parent_exit_test(Node, true),
    stop_node(Peer, Node),
    {comment, C1 ++ " " ++ C2}.

spawn_request_link_parent_exit_test(Node, CompoundExitReason) ->
    %% Early parent exit...
    Tester = self(),

    ExitReason = if CompoundExitReason -> "kaboom";
                    true -> kaboom
                 end,

    verify_nc(node()),

    %% Ensure code loaded on other node...
    _ = rpc:call(Node, ?MODULE, module_info, []),

    ParentFun = case node() == Node of
                    true ->
                        fun (Wait) ->
                                spawn_request(?MODULE, spawn_request_test_exit_child,
                                              [Tester], [link,{priority,max}]),
                                receive after Wait -> ok end,
                                exit(ExitReason)
                        end;
                    false ->
                        fun (Wait) ->
                                spawn_request(Node, ?MODULE,
                                              spawn_request_test_exit_child,
                                              [Tester], [link,{priority,max}]),
                                receive after Wait -> ok end,
                                exit(ExitReason)
                        end
                end,
    lists:foreach(fun (N) ->
                          spawn_opt(fun () ->
                                            %% Give parent some work to do when
                                            %% exiting and by this increase
                                            %% possibilities for races...
                                            T = ets:new(x,[]),
                                            ets:insert(T, lists:map(fun (I) ->
                                                                            {I,I}
                                                                    end,
                                                                    lists:seq(1,10000))),
                                            ParentFun(N rem 10) end,
                                    [{priority, max}])
                  end,
                  lists:seq(1, 10000)),
    N = gather_parent_exits(ExitReason, false),
    CFs = erpc:call(Node,
                    fun () ->
                            %% Ensure all children have had time to enter an exiting state...
                            receive after 100*test_server:timetrap_scale_factor() -> ok end,
                            lists:map(fun (P) ->
                                              {P, process_info(P, current_function)}
                                      end, processes())
                    end),
    lists:foreach(fun ({P, {current_function, {?MODULE, spawn_request_test_exit_child, 1}}}) ->
                          ct:fail({missing_exit_to_child_detected, P});
                      (_) ->
                          ok
                  end, CFs),
    Comment =
        "Got "
        ++ integer_to_list(N)
        ++ if node() == Node -> " node local";
                  true -> " node remote"
           end
        ++ if CompoundExitReason -> " \"kaboom\"";
              true -> " \'kaboom\'"
           end
        ++ " exits!",
    erlang:display(Comment),
    Comment.

spawn_request_link_parent_exit_nodedown(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(#{connection => 0}),
    N = 1000,
    ExitCounter = spawn(Node, fun exit_counter/0),
    lists:foreach(fun (_) ->
                          spawn_request_link_parent_exit_nodedown_test(Node)
                  end,
                  lists:seq(1, N)),
    ResRef = make_ref(),
    ExitCounter ! {get_results, self(), ResRef},
    Cmnt = receive
               {ResRef, CntMap} ->
                   lists:flatten(
                     ["In total ", integer_to_list(N), " exits. ",
                      "Detected exits: ",
                      maps:fold(fun (ExitReason, Count, "") ->
                                        io_lib:format("~p exit ~p times",
                                                      [ExitReason, Count]);
                                    (ExitReason, Count, Acc) ->
                                        [Acc, io_lib:format("; ~p exit ~p times",
                                                            [ExitReason, Count])]
                                end,
                                "",
                                CntMap),
                      "."])
           end,
    io:format("~s~n", [Cmnt]),
    stop_node(Peer, Node),
    {comment, Cmnt}.

exit_counter() ->
    true = register(exit_counter, self()),
    exit_counter(#{}).

exit_counter(CntMap) ->
    receive
        {get_results, From, Ref} ->
            From ! {Ref, CntMap};
        {exit, Reason} ->
            OldCnt = maps:get(Reason, CntMap, 0),
            exit_counter(CntMap#{Reason => OldCnt+1})
    end.

spawn_request_link_parent_exit_nodedown_test(Node) ->
    pong = net_adm:ping(Node),
    ChildFun = fun () ->
                       process_flag(trap_exit, true),
                       receive
                           {'EXIT', _, Reason} ->
                               exit_counter ! {exit, Reason}
                       end
               end,
    {Pid, Mon} = spawn_monitor(fun () ->
                                       _ReqID = spawn_request(Node,
                                                              ChildFun,
                                                              [link,
                                                               {priority,max}]),
                                       exit(bye)
                               end),
    receive
        {'DOWN', Mon, process, Pid, Reason} ->
            bye = Reason,
            erlang:disconnect_node(Node)
    end,
    ok.


spawn_request_abandon_bif(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    false = spawn_request_abandon(make_ref()),
    false = spawn_request_abandon(spawn_request(fun () -> ok end)),
    false = spawn_request_abandon(rpc:call(Node, erlang, make_ref, [])),
    try
        noreturn = spawn_request_abandon(self()) 
    catch
        error:badarg ->
            ok
    end,
    try
        noreturn = spawn_request_abandon(4711)
    catch
        error:badarg ->
            ok
    end,

    verify_nc(node()),

    %% Ensure code loaded on other node...
    _ = rpc:call(Node, ?MODULE, module_info, []),


    TotOps = 1000,
    Tester = self(),

    ParentFun = fun (Wait, Opts) ->
                        ReqId = spawn_request(Node, ?MODULE,
                                              spawn_request_test_exit_child,
                                              [Tester], Opts),
                        receive after Wait -> ok end,
                        case spawn_request_abandon(ReqId) of
                            true ->
                                ok;
                            false ->
                                receive
                                    {spawn_reply, ReqId, error, _} ->
                                        exit(spawn_failed);
                                    {spawn_reply, ReqId, ok, Pid} ->
                                        unlink(Pid),
                                        exit(Pid, bye)
                                after
                                    0 ->
                                        exit(missing_spawn_reply)
                                end
                        end
                end,
    %% Parent exit early...
    lists:foreach(fun (N) ->
                          spawn_opt(fun () ->
                                            ParentFun(N rem 50, [link])
                                    end, [link,{priority,max}])
                  end,
                  lists:seq(1, TotOps)),
    NoA1 = gather_parent_exits(abandoned, true),
    %% Parent exit late...
    lists:foreach(fun (N) ->
                          spawn_opt(fun () ->
                                            ParentFun(N rem 50, [link]),
                                            receive
                                                {spawn_reply, _, _, _} ->
                                                    exit(unexpected_spawn_reply)
                                            after
                                                1000 -> ok
                                            end
                                    end, [link,{priority,max}])
                  end,
                  lists:seq(1, TotOps)),
    NoA2 = gather_parent_exits(abandoned, true),
    CFs = erpc:call(Node,
                    fun () ->
                            %% Ensure all children have had time to enter an exiting state...
                            receive after 100*test_server:timetrap_scale_factor() -> ok end,
                            lists:map(fun (P) ->
                                              {P, process_info(P, current_function)}
                                      end, processes())
                    end),
    lists:foreach(fun ({P, {current_function, {?MODULE, spawn_request_test_exit_child, 1}}}) ->
                          ct:fail({missing_exit_to_child_detected, P});
                      (_) ->
                          ok
                  end, CFs),
    %% Parent exit early...
    lists:foreach(fun (N) ->
                          spawn_opt(fun () ->
                                            ParentFun(N rem 50, [])
                                    end, [link,{priority,max}])
                  end,
                  lists:seq(1, TotOps)),
    0 = gather_parent_exits(abandoned, true),
    %% Parent exit late...
    lists:foreach(fun (N) ->
                          spawn_opt(fun () ->
                                            ParentFun(N rem 50, []),
                                            receive
                                                {spawn_reply, _, _, _} ->
                                                    exit(unexpected_spawn_reply)
                                            after
                                                1000 -> ok
                                            end
                                    end, [link,{priority,max}])
                  end,
                  lists:seq(1, TotOps)),
    0 = gather_parent_exits(abandoned, true),
    stop_node(Peer, Node),
    C = "Got " ++ integer_to_list(NoA1) ++ " and "
        ++ integer_to_list(NoA2) ++ " abandoneds of 2*"
        ++ integer_to_list(TotOps) ++ " ops!",
    true = NoA1 /= 0,
    true = NoA1 /= TotOps,
    true = NoA2 /= 0,
    true = NoA2 /= TotOps,
    {comment, C}.

spawn_request_test_exit_child(Tester) ->
  Child = self(),
  _ = spawn_opt(fun () ->
                        process_flag(trap_exit, true),
                        receive
                            {'EXIT', Child, Reason} ->
                                Tester ! {parent_exit, Reason}
                        end
                end, [link,{priority,max}]),
  receive after infinity -> ok end.

gather_parent_exits(Reason, AllowOther) ->
    receive after 2000 -> ok end,
    gather_parent_exits(Reason, AllowOther, 0).

gather_parent_exits(Reason, AllowOther, N) ->
    receive
        {parent_exit, Reason} ->
            gather_parent_exits(Reason, AllowOther, N+1);
        {parent_exit, _} = ParentExit ->
            case AllowOther of
                false ->
                    ct:fail(ParentExit);
                true ->
                    gather_parent_exits(Reason, AllowOther, N)
            end
    after 0 ->
            N
    end.
dist_spawn_monitor(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    R1 = spawn_request(Node, erlang, exit, [hej], [monitor]),
    receive
        {spawn_reply, R1, ok, P1} ->
            receive
                {'DOWN', R1, process, P1, Reason1} ->
                    hej = Reason1
            end
    end,
    {P2, Mon2} = spawn_monitor(Node, erlang, exit, [hej]),
    receive
        {'DOWN', Mon2, process, P2, Reason2} ->
            hej = Reason2
    end,
    {P3, Mon3} = spawn_opt(Node, erlang, exit, [hej], [monitor]),
    receive
        {'DOWN', Mon3, process, P3, Reason3} ->
            hej = Reason3
    end,
    stop_node(Peer, Node),
    ok.

spawn_against_ei_node(Config) when is_list(Config) ->
    %% Spawn against an ei node which does not support spawn
    {ok, EiNode} = start_ei_node(Config),
    try
        %% First spawn triggering a new connection; which
        %% will trigger hopeful data transcoding
        %% of spawn requests...
        io:format("~n~nDoing initial connect tests...~n", []),
        spawn_ei_node_test(EiNode, true),
        %% Spawns on an already existing connection...
        io:format("~n~nDoing already connected tests...~n", []),
        spawn_ei_node_test(EiNode, false),
        ok
    after
        ok = stop_ei_node(EiNode)
    end.

spawn_against_old_node(Config) when is_list(Config) ->
    %% Same spawn tests against a two releases old node as against
    %% ei node above
    OldRel = integer_to_list(list_to_integer(erlang:system_info(otp_release))-2),
    OldRelName = OldRel ++ "_latest",
    %% We clear all ERL_FLAGS for the old node as all options may not
    %% be supported.
    ClearEnv = lists:foldl(
                 fun({Key,_Value}, Acc) ->
                         case re:run(Key,"^ERL_.*FLAGS$") of
                             {match,_} ->
                                 [{Key,""}|Acc];
                             nomatch ->
                                 Acc
                         end
                 end, [], os:env()),
    case ?CT_PEER_REL(#{connection => 0, env => ClearEnv },
                      OldRelName,
                      proplists:get_value(priv_dir, Config)) of
	not_available ->
            {skipped, "No OTP "++OldRel++" available"};
        {ok, Peer, OldNode} ->
            try
                %% Spawns triggering a new connection; which
                %% will trigger hopeful data transcoding
                %% of spawn requests...
                io:format("~n~nDoing initial connect tests...~n", []),
                spawn_node_test(OldNode, true),
                io:format("~n~nDoing already connected tests...~n", []),
                %% Spawns on an already existing connection...
                spawn_node_test(OldNode, false),
                ok
            after
                peer:stop(Peer)
            end
    end.

spawn_against_new_node(Config) when is_list(Config) ->
    {ok, Peer, CurrNode} = ?CT_PEER(#{connection => 0}),
    try
        %% Spawns triggering a new connection; which
        %% will trigger hopeful data transcoding
        %% of spawn requests...
        io:format("~n~nDoing initial connect tests...~n", []),
        spawn_node_test(CurrNode, true),
        io:format("~n~nDoing already connected tests...~n", []),
        %% Spawns on an already existing connection...
        spawn_node_test(CurrNode, false),
        ok
    after
        peer:stop(Peer)
    end.

spawn_ei_node_test(Node, Disconnect) ->
    io:format("Testing spawn_request() against ei node...", []),
    disconnect_node(Node, Disconnect),
    Ref1 = spawn_request(Node, erlang, exit, [hej], [monitor, {reply_tag, a_tag}]),
    receive
        {a_tag, Ref1, Err, Notsup} ->
            error = Err,
            notsup = Notsup,
            ok
    end,
    io:format("Testing spawn_monitor() against ei node...", []),
    disconnect_node(Node, Disconnect),
    try
        spawn_monitor(Node, erlang, exit, [hej])
    catch
        error:notsup ->
            ok
    end,
    io:format("Testing spawn_opt() with monitor against ei node...", []),
    disconnect_node(Node, Disconnect),
    {P0, M0} = spawn_opt(Node, erlang, exit, [hej], [monitor]),
    receive
        {'DOWN', M0, process, P0, R0} ->
            notsup = R0
    end,
    io:format("Testing spawn_opt() with link against ei node...", []),
    disconnect_node(Node, Disconnect),
    process_flag(trap_exit, true),
    P1 = spawn_opt(Node, erlang, exit, [hej], [link]),
    receive
        {'EXIT', P1, R1} ->
            notsup = R1
    end,
    io:format("Testing spawn_link() against ei node...", []),
    disconnect_node(Node, Disconnect),
    P2 = spawn_link(Node, erlang, exit, [hej]),
    receive
        {'EXIT', P2, R2} ->
            notsup = R2
    end,
    ok.

disconnect_node(_Node, false) ->
    ok;
disconnect_node(Node, true) ->
    lists:member(Node, nodes([visible, hidden])) andalso begin
        monitor_node(Node, true),
        true = erlang:disconnect_node(Node),
        receive {nodedown, Node} -> ok end
    end.

spawn_node_test(Node, Disconnect) ->
    io:format("Testing spawn_request()...", []),
    disconnect_node(Node, Disconnect),
    R1 = spawn_request(Node, erlang, exit, [hej], [monitor, {reply_tag, a_tag}]),
    receive
        {a_tag, R1, ok, P1} ->
            Node = node(P1),
            receive
                {'DOWN', R1, process, P1, hej} -> ok
            end
    end,
    io:format("Testing spawn_monitor()...", []),
    disconnect_node(Node, Disconnect),
    {P2, M2} = spawn_monitor(Node, erlang, exit, [hej]),
    receive
        {'DOWN', M2, process, P2, hej} -> ok
    end,
    Node = node(P2),
    io:format("Testing spawn_opt() with monitor...", []),
    disconnect_node(Node, Disconnect),
    {P3, M3} = spawn_opt(Node, erlang, exit, [hej], [monitor]),
    receive
        {'DOWN', M3, process, P3, hej} -> ok
    end,
    Node = node(P3),
    io:format("Testing spawn_opt() with link...", []),
    disconnect_node(Node, Disconnect),
    process_flag(trap_exit, true),
    P4 = spawn_opt(Node, erlang, exit, [hej], [link]),
    Node = node(P4),
    receive
        {'EXIT', P4, hej} ->
            ok
    end,
    io:format("Testing spawn_link()...", []),
    disconnect_node(Node, Disconnect),
    P5 = spawn_link(Node, erlang, exit, [hej]),
    Node = node(P5),
    receive
        {'EXIT', P5, hej} ->
            ok
    end.

spawn_request_reply_option(Config) when is_list(Config) ->
    spawn_request_reply_option_test(undefined, node()),
    {ok, Peer, Node} = ?CT_PEER(),
    spawn_request_reply_option_test(Peer, Node).
    
spawn_request_reply_option_test(Peer, Node) ->
    io:format("Testing on node: ~p~n", [Node]),
    Parent = self(),
    Done1 = make_ref(),
    RID1 = spawn_request(Node, fun () -> Parent ! Done1 end, [{reply, yes}]),
    receive Done1 -> ok end,
    receive
        {spawn_reply, RID1, ok, _} -> ok
    after 0 ->
            ct:fail(missing_spawn_reply)
    end,
    Done2 = make_ref(),
    RID2 = spawn_request(Node, fun () -> Parent ! Done2 end, [{reply, success_only}]),
    receive Done2 -> ok end,
    receive
        {spawn_reply, RID2, ok, _} -> ok
    after 0 ->
            ct:fail(missing_spawn_reply)
    end,
    Done3 = make_ref(),
    RID3 = spawn_request(Node, fun () -> Parent ! Done3 end, [{reply, error_only}]),
    receive Done3 -> ok end,
    receive
        {spawn_reply, RID3, _, _} ->
            ct:fail(unexpected_spawn_reply)
    after 0 ->
            ok
    end,
    Done4 = make_ref(),
    RID4 = spawn_request(Node, fun () -> Parent ! Done4 end, [{reply, no}]),
    receive Done4 -> ok end,
    receive
        {spawn_reply, RID4, _, _} ->
            ct:fail(unexpected_spawn_reply)
    after 0 ->
            ok
    end,
    RID5 = spawn_request(Node, fun () -> ok end, [{reply, yes}, bad_option]),
    receive
        {spawn_reply, RID5, error, badopt} -> ok
    end,
    RID6 = spawn_request(Node, fun () -> ok end, [{reply, success_only}, bad_option]),
    receive
        {spawn_reply, RID6, error, badopt} -> ct:fail(unexpected_spawn_reply)
    after 1000 -> ok
    end,
    RID7 = spawn_request(Node, fun () -> ok end, [{reply, error_only}, bad_option]),
    receive
        {spawn_reply, RID7, error, badopt} -> ok
    end,
    RID8 = spawn_request(Node, fun () -> ok end, [{reply, no}, bad_option]),
    receive
        {spawn_reply, RID8, error, badopt} -> ct:fail(unexpected_spawn_reply)
    after 1000 -> ok
    end,
    RID8_1 = spawn_request(Node, fun () -> ok end, [{reply, nahh}]),
    receive
        {spawn_reply, RID8_1, error, badopt} -> ok
    end,
    case Node == node() of
        true ->
            ok;
        false ->
            stop_node(Peer, Node),
            RID9 = spawn_request(Node, fun () -> ok end, [{reply, yes}]),
            receive
                {spawn_reply, RID9, error, noconnection} -> ok
            end,
            RID10 = spawn_request(Node, fun () -> ok end, [{reply, success_only}]),
            receive
                {spawn_reply, RID10, error, noconnection} -> ct:fail(unexpected_spawn_reply)
            after 1000 -> ok
            end,
            RID11 = spawn_request(Node, fun () -> ok end, [{reply, error_only}]),
            receive
                {spawn_reply, RID11, error, noconnection} -> ok
            end,
            RID12 = spawn_request(Node, fun () -> ok end, [{reply, no}]),
            receive
                {spawn_reply, RID12, error, noconnection} -> ct:fail(unexpected_spawn_reply)
            after 1000 -> ok
            end,
            ok
    end.

dist_spawn_arg_list_mixup(Config) when is_list(Config) ->
    %% A process newly spawned via the distribution is passed the
    %% argument list to use as the first message followed by an intialization
    %% message. Those two messages *must* be the first messages in its queue
    %% when it begins execution. The parallel receive/send signal optimization
    %% could potentially cause reordering of messages if certain future
    %% changes are made. This test case tries to cause a situation where a
    %% message reordering potentially could happen, and hopefully will detect
    %% such problematic changes.
    Tester = self(),
    NoScheds = 8,
    NoSchedsStr = integer_to_list(NoScheds),
    NoSchedsList = lists:seq(1, NoScheds),
    {ok, Peer, Node} = ?CT_PEER(["+S"++NoSchedsStr++":"++NoSchedsStr]),
    AttackMsg = make_ref(),
    AttackArgList = [Tester, AttackMsg],
    OkMsg = make_ref(),
    As = lists:map(
           fun (_) ->
                   spawn_opt(
                     Node,
                     fun () ->
                             dist_spawn_arg_list_mixup_sender(AttackArgList,
                                                              1000)
                     end, [{priority, high}, link])
           end, NoSchedsList),
    Relay = spawn_opt(
              Node,
              fun () ->
                      receive
                          {attack, Victim} ->
                              lists:foreach(fun (A) ->
                                                    A ! {attack, Victim}
                                            end, As)
                      end
              end, [{priority, max}, link]),
    receive after 100 -> ok end,
    Victim = spawn_opt(Node, erlang, send, [Tester, OkMsg],
                       [{message_queue_data, off_heap},
                        {priority, normal},
                        link]),
    Relay ! {attack, Victim},
    receive
        OkMsg ->
            ok;
        AttackMsg ->
            ct:fail(child_process_used_message_as_argument_list)
    end,
    lists:foreach(fun (P) ->
                          unlink(P)
                  end, [Victim] ++ [Relay] ++ As),
    peer:stop(Peer),
    ok.

dist_spawn_arg_list_mixup_sender(Msg, N) ->
    receive
        {attack, Victim} ->
            dist_spawn_arg_list_mixup_sender(Victim, Msg, N)
    after
        0 ->
            dist_spawn_arg_list_mixup_sender(Msg, N)
    end.

dist_spawn_arg_list_mixup_sender(_Pid, _Msg, 0) ->
    ok;
dist_spawn_arg_list_mixup_sender(Pid, Msg, N) ->
    Pid ! Msg,
    dist_spawn_arg_list_mixup_sender(Pid, Msg, N-1).

processes_term_proc_list(Config) when is_list(Config) ->
    Tester = self(),

    Run = fun(Args) ->
              {ok, Peer, Node} = ?CT_PEER(Args),
              RT = spawn_link(Node, fun () ->
                              receive after 1000 -> ok end,
                              as_expected = processes_term_proc_list_test(false),
                              Tester ! {it_worked, self()}
                      end),
              receive {it_worked, RT} -> ok end,
              stop_node(Peer, Node)
          end,

    %% We have to run this test case with +S1 since instrument:allocations()
    %% will report a free()'d block as present until it's actually deallocated
    %% by its employer.
    Run(["+MSe", "true", "+Muatags", "false", "+S1"]),
    Run(["+MSe", "true", "+Muatags", "true", "+S1"]),

    ok.

processes_iter(Config) when is_list(Config) ->
    ProcessLimit = erlang:system_info(process_limit),
    {'EXIT',{badarg,_}} = catch erts_internal:processes_next(ProcessLimit + 1),
    {'EXIT',{badarg,_}} = catch erts_internal:processes_next(-1),
    {'EXIT',{badarg,_}} = catch erts_internal:processes_next(1 bsl 32),
    {'EXIT',{badarg,_}} = catch erts_internal:processes_next(1 bsl 64),
    {'EXIT',{badarg,_}} = catch erts_internal:processes_next(abc),

    none = erts_internal:processes_next(ProcessLimit),

    ok.


%% OTP-18322: Send msg to spawning process pid returned from processes/0
processes_send_infant(_Config) ->
    case erlang:system_info(schedulers_online) of
        1 ->
            {skip, "Only one scheduler online"};
        NScheds ->
            processes_send_infant_do(NScheds)
    end.

processes_send_infant_do(NScheds) ->
    IgnoreList = erlang:processes(),
    IgnorePids = maps:from_keys(IgnoreList, ignore),
    Tester = self(),

    %% To provoke bug we need sender and spawner on different schedulers.
    %% Let spawners use schedulers nr 2 to NScheds
    NSpawnerScheds = NScheds - 1,
    NSpawners = 2 * NSpawnerScheds,
    [spawn_link(fun() ->
                        processes_send_infant_spawner((I rem  NSpawnerScheds) + 2,
                                                      Tester, Tester, 1)
                end)
     || I <- lists:seq(0, NSpawners-1)],

    %% and make sure sender use scheduler 1
    {Sender,SenderMon} =
        spawn_opt(
          fun() ->
                  timeout = processes_send_infant_loop(IgnorePids)
          end,
          [link, monitor, {scheduler,1}]),

    %% Run test for a little while and see if VM crashes
    {ok, _TRef} = timer:send_after(1000, Sender, timeout),
    {'DOWN', SenderMon, process, Sender, normal} = receive_any(),

    %% Stop spawners and collect stats
    processes_send_infant_broadcast(erlang:processes(),
                                    {processes_send_infant, stop},
                                    IgnorePids),
    {TotSpawn, TheLastOfUs} =
        lists:foldl(fun(_, {SpawnCnt, Pids}) ->
                            {Pid, Generation} = receive_any(),
                            io:format("Got ~p from ~p\n", [Generation, Pid]),
                            {SpawnCnt+Generation, [Pid | Pids]}
                    end,
                    {0, []},
                    lists:seq(1, NSpawners)),
    io:format("Total spawned processes: ~p\n", [TotSpawn]),
    Aliens = (erlang:processes() -- IgnoreList) -- TheLastOfUs,
    io:format("Alien processes: ~p\n", [Aliens]),
    ok.



processes_send_infant_loop(IgnorePids) ->
    %% Send message identifying this test case, in case we send
    %% to alien processes spawned during the test.
    Msg = processes_send_infant,
    processes_send_infant_broadcast(erlang:processes(),
                                    Msg,
                                    IgnorePids),
    receive timeout -> timeout
    after 0 ->
            processes_send_infant_loop(IgnorePids)
    end.

processes_send_infant_broadcast([Pid | Tail], Msg, IgnorePids) ->
    case maps:is_key(Pid, IgnorePids) of
        false ->
            Pid ! Msg;
        true ->
            ignore
    end,
    processes_send_infant_broadcast(Tail, Msg, IgnorePids);
processes_send_infant_broadcast([], _, _) ->
    ok.

processes_send_infant_spawner(Sched, Tester, Parent, Generation) ->
    link(Tester),
    case receive_any() of
        processes_send_infant ->
            case Parent of
                Tester -> ok;
                _ -> Parent ! {die, self()}
            end,
            Self = self(),
            Child = spawn_opt(fun() ->
                                      processes_send_infant_spawner(Sched, Tester,
                                                                    Self,
                                                                    Generation+1)
                              end,
                             [{message_queue_data, off_heap},
                              {scheduler, Sched}]),
            process_send_infant_spawner_epilogue(Child);

        {processes_send_infant, stop} ->
            Tester ! {self(), Generation}
    end.

process_send_infant_spawner_epilogue(Child) ->
    %% Parent stays alive only to ensure child gets stop message
    case receive_any() of
        processes_send_infant ->
            process_send_infant_spawner_epilogue(Child);
        {die, Child} ->
            ok;
        {processes_send_infant, stop}=Stop ->
            %% We are not sure child was spawned when stop message was sent
            %% so we relay it.
            Child ! Stop
    end.


-define(CHK_TERM_PROC_LIST(MC, XB),
	chk_term_proc_list(?LINE, MC, XB)).

chk_term_proc_list(Line, MustChk, ExpectBlks) ->
    Allocs = instrument:allocations(),
    case {MustChk, Allocs} of
	{false, {error, not_enabled}} ->
	    not_enabled;
	{false, {ok, {_Shift, _Unscanned, ByOrigin}}} when ByOrigin =:= #{} ->
	    not_enabled;
	{_, {ok, {_Shift, _Unscanned, ByOrigin}}} ->
            ByType = maps:get(system, ByOrigin, #{}),
            Hist = maps:get(ptab_list_deleted_el, ByType, {}),
	    case lists:sum(tuple_to_list(Hist)) of
		ExpectBlks ->
                    ok;
		Blks ->
                    exit({line, Line, mismatch,
                          expected, ExpectBlks,
                          actual, Blks})
	    end
    end,
    ok.

processes_term_proc_list_test(MustChk) ->
    Tester = self(),
    enable_internal_state(),
    PBInfo = erts_debug:get_internal_state(processes_bif_info),
    print_processes_bif_info(PBInfo),
    WantReds = PBInfo#ptab_list_bif_info.min_start_reds + 10,
    #ptab_list_bif_info{tab_chunks = Chunks,
	tab_chunks_size = ChunksSize,
	tab_indices_per_red = IndiciesPerRed
    } = PBInfo,
    true = Chunks > 1,
    true = Chunks*ChunksSize >= IndiciesPerRed*WantReds,
    Processes = fun () ->
	    erts_debug:set_internal_state(reds_left,
		WantReds),
	    processes()
    end,
    Exit = fun (P) ->
	    unlink(P),
	    exit(P, bang),
	    wait_until(
		fun () ->
			not lists:member(
			    P,
			    erts_debug:get_internal_state(
				processes))
		end)
    end,
    SpawnSuspendProcessesProc = fun () ->
		  erlang:system_flag(multi_scheduling, block_normal),
		  P = spawn_link(fun () ->
					 Tester ! {suspend_me, self()},
					 Tester ! {self(),
						   done,
						   hd(Processes())},
					 receive after infinity -> ok end
				 end),
		  receive {suspend_me, P} -> ok end,
		  erlang:suspend_process(P),
		  erlang:system_flag(multi_scheduling, unblock_normal),
		  [{status,suspended},
		   {current_function,{erlang,ptab_list_continue,2}}]
		      = process_info(P, [status, current_function]),
		  P
	  end,
    ResumeProcessesProc = fun (P) ->
					erlang:resume_process(P),
					receive {P, done, _} -> ok end
				end,
    ?CHK_TERM_PROC_LIST(MustChk, 0),
    HangAround = fun () -> receive after infinity -> ok end end,
    HA1 = spawn_link(HangAround),
    HA2 = spawn_link(HangAround),
    HA3 = spawn_link(HangAround),
    S1 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 1),
    Exit(HA1),
    ?CHK_TERM_PROC_LIST(MustChk, 2),
    S2 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 3),
    S3 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 4),
    Exit(HA2),
    ?CHK_TERM_PROC_LIST(MustChk, 5),
    S4 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 6),
    Exit(HA3),
    ?CHK_TERM_PROC_LIST(MustChk, 7),
    ResumeProcessesProc(S1),
    ?CHK_TERM_PROC_LIST(MustChk, 5),
    ResumeProcessesProc(S3),
    ?CHK_TERM_PROC_LIST(MustChk, 4),
    ResumeProcessesProc(S4),
    ?CHK_TERM_PROC_LIST(MustChk, 3),
    ResumeProcessesProc(S2),
    ?CHK_TERM_PROC_LIST(MustChk, 0),
    Exit(S1),
    Exit(S2),
    Exit(S3),
    Exit(S4),


    HA4 = spawn_link(HangAround),
    HA5 = spawn_link(HangAround),
    HA6 = spawn_link(HangAround),
    S5 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 1),
    Exit(HA4),
    ?CHK_TERM_PROC_LIST(MustChk, 2),
    S6 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 3),
    Exit(HA5),
    ?CHK_TERM_PROC_LIST(MustChk, 4),
    S7 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 5),
    Exit(HA6),
    ?CHK_TERM_PROC_LIST(MustChk, 6),
    S8 = SpawnSuspendProcessesProc(),
    ?CHK_TERM_PROC_LIST(MustChk, 7),

    erlang:system_flag(multi_scheduling, block_normal),
    Exit(S8),
    ?CHK_TERM_PROC_LIST(MustChk, 7),
    Exit(S5),
    ?CHK_TERM_PROC_LIST(MustChk, 6),
    Exit(S7),
    ?CHK_TERM_PROC_LIST(MustChk, 6),
    Exit(S6),
    ?CHK_TERM_PROC_LIST(MustChk, 0),
    erlang:system_flag(multi_scheduling, unblock_normal),
    as_expected.


otp_7738_waiting(Config) when is_list(Config) ->
    otp_7738_test(waiting).

otp_7738_suspended(Config) when is_list(Config) ->
    otp_7738_test(suspended).

otp_7738_resume(Config) when is_list(Config) ->
    otp_7738_test(resume).

otp_7738_test(Type) ->
    sys_mem_cond_run(3072, fun () -> do_otp_7738_test(Type) end).

do_otp_7738_test(Type) ->
    T = self(),
    S = spawn_link(fun () ->
		receive
		    {suspend, Suspendee} ->
			erlang:suspend_process(Suspendee),
			T ! {suspended, Suspendee},
			receive
			after 10 ->
				erlang:resume_process(Suspendee),
				Suspendee ! wake_up
			end;
		    {send, To, Msg} ->
			receive after 10 -> ok end,
			To ! Msg
		end
	end),
    R = spawn_link(fun () ->
		X = lists:seq(1, 20000000),
		T ! {initialized, self()},
		case Type of
		    _ when Type == suspended;
		Type == waiting ->
		    receive _ -> ok end;
		_ when Type == resume ->
		    Receive = fun (F) ->
			    receive
				_ ->
				    ok
			    after 0 ->
				    F(F)
			    end
		    end,
		    Receive(Receive)
	    end,
	    T ! {woke_up, self()},
	    id(X)
    end),
    receive {initialized, R} -> ok end,
    receive after 10 -> ok end,
    case Type of
	      suspended ->
		  erlang:suspend_process(R),
		  S ! {send, R, wake_up};
	      waiting ->
		  S ! {send, R, wake_up};
	      resume ->
		  S ! {suspend, R},
		  receive {suspended, R} -> ok end
	  end,
    erlang:garbage_collect(R),
    case Type of
	      suspended ->
		  erlang:resume_process(R);
	      _ ->
		  ok
	  end,
    receive
	      {woke_up, R} ->
		  ok
	  after 2000 ->
		  I = process_info(R, [status, message_queue_len]),
		  io:format("~p~n", [I]),
		  ct:fail(no_progress)
	  end,
    ok.

gor(Reds, Stop) ->
    receive
	drop_me ->
	    gor(Reds+1, Stop);	    
	{From, reds} ->
	    From ! {reds, Reds, self()},
	    gor(Reds+1, Stop);
	{From, Stop} ->
	    From ! {stopped, Stop, Reds, self()}
    after 0 ->
	    gor(Reds+1, Stop)
    end.

garb_other_running(Config) when is_list(Config) ->
    Stop = make_ref(),
    {Pid, Mon} = spawn_monitor(fun () -> gor(0, Stop) end),
    Reds = lists:foldl(fun (N, OldReds) ->
			             case N rem 2 of
					 0 -> Pid ! drop_me;
					 _ -> ok
				     end,
				     erlang:garbage_collect(Pid),
				     receive after 1 -> ok end,
				     Pid ! {self(), reds},
				     receive
					       {reds, NewReds, Pid} ->
						   true = (NewReds > OldReds),
						   NewReds
					   end
			     end,
			     0,
			     lists:seq(1, 10000)),
    receive after 1 -> ok end,
    Pid ! {self(), Stop},
    receive
	      {stopped, Stop, StopReds, Pid} ->
		  true = (StopReds > Reds)
	  end,
    receive {'DOWN', Mon, process, Pid, normal} -> ok end,
    ok.

no_priority_inversion(Config) when is_list(Config) ->
    Prio = process_flag(priority, max),
    Master = self(),
    Executing = make_ref(),
    HTLs = lists:map(fun (Sched) ->
			     spawn_opt(fun () ->
                                               Master ! {self(), Executing},
					       tok_loop()
				       end,
				       [{priority, high},
                                        {scheduler, Sched},
                                        monitor,
                                        link])
		     end,
		     lists:seq(1, erlang:system_info(schedulers_online))),
    lists:foreach(fun ({P, _}) -> receive {P,Executing} -> ok end end, HTLs),
    LTL = spawn_opt(fun () ->
			    tok_loop()
		    end,
		    [{priority, low}, monitor, link]),
    false = erlang:check_process_code(element(1, LTL), nonexisting_module),
    true = erlang:garbage_collect(element(1, LTL)),
    lists:foreach(fun ({P, _}) ->
			  unlink(P),
			  exit(P, kill)
		  end, [LTL | HTLs]),
    lists:foreach(fun ({P, M}) ->
			  receive
			      {'DOWN', M, process, P, killed} ->
				  ok
			  end
		  end, [LTL | HTLs]),
    process_flag(priority, Prio),
    ok.

no_priority_inversion2(Config) when is_list(Config) ->
    Prio = process_flag(priority, max),
    Master = self(),
    Executing = make_ref(),
    MTLs = lists:map(fun (Sched) ->
			     spawn_opt(fun () ->
                                               Master ! {self(), Executing},
					       tok_loop()
				       end,
				       [{priority, max},
                                        {scheduler, Sched},
                                        monitor, link])
		     end,
		     lists:seq(1, erlang:system_info(schedulers_online))),
    lists:foreach(fun ({P, _}) -> receive {P,Executing} -> ok end end, MTLs),
    {PL, ML} = spawn_opt(fun () ->
			       tok_loop()
		       end,
		       [{priority, low}, monitor, link]),
    RL = request_test(PL, low),
    RN = request_test(PL, normal),
    RH = request_test(PL, high),
    receive
	{system_task_test, _, _} ->
	    ct:fail(unexpected_system_task_completed)
    after 1000 ->
	    ok
    end,
    RM = request_test(PL, max),
    receive
	{system_task_test, RM, true} ->
	    ok
    end,
    lists:foreach(fun ({P, _}) ->
			  unlink(P),
			  exit(P, kill)
		  end, MTLs),
    lists:foreach(fun ({P, M}) ->
			  receive
			      {'DOWN', M, process, P, killed} ->
				  ok
			  end
		  end, MTLs),
    receive
	{system_task_test, RH, true} ->
	    ok
    end,
    receive
	{system_task_test, RN, true} ->
	    ok
    end,
    receive
	{system_task_test, RL, true} ->
	    ok
    end,
    unlink(PL),
    exit(PL, kill),
    receive
	{'DOWN', ML, process, PL, killed} ->
	    ok
    end,
    process_flag(priority, Prio),
    ok.

request_test(Pid, Prio) ->
    Ref = make_ref(),
    erts_internal:request_system_task(Pid, Prio, {system_task_test, Ref}),
    Ref.

system_task_blast(Config) when is_list(Config) ->
    Me = self(),
    GCReq = fun () ->
		    RL = test_req(Me, 100),
		    lists:foreach(fun (R) ->
					  receive
					      {system_task_test, R, true} ->
						  ok
					  end
				  end, RL),
		    exit(it_worked)
	    end,
    HTLs = lists:map(fun (_) -> spawn_monitor(GCReq) end, lists:seq(1, 1000)),
    lists:foreach(fun ({P, M}) ->
			  receive
			      {'DOWN', M, process, P, it_worked} ->
				  ok
			  end
		  end, HTLs),
    ok.

test_req(_Pid, 0) ->
    [];
test_req(Pid, N) ->
    R0 = request_test(Pid, low),
    R1 = request_test(Pid, normal),
    R2 = request_test(Pid, high),
    R3 = request_test(Pid, max),
    [R0, R1, R2, R3 | test_req(Pid, N-1)].

system_task_on_suspended(Config) when is_list(Config) ->
    {P, M} = spawn_monitor(fun () ->
				   tok_loop()
			   end),
    true = erlang:suspend_process(P),
    {status, suspended} = process_info(P, status),
    true = erlang:garbage_collect(P),
    {status, suspended} = process_info(P, status),
    true = erlang:resume_process(P),
    false = ({status, suspended} == process_info(P, status)),
    exit(P, kill),
    receive
	{'DOWN', M, process, P, killed} ->
	    ok
    end.

%% When a system task couldn't be enqueued due to the process being in an
%% incompatible state, it would linger in the system task list and get executed
%% anyway the next time the process was scheduled. This would result in a
%% double-free at best.
%%
%% This test continuously purges modules while other processes run dirty code,
%% which will provoke this error as ERTS_PSTT_CPC can't be enqueued while a
%% process is running dirty code.
system_task_failed_enqueue(Config) when is_list(Config) ->
    case erlang:system_info(dirty_cpu_schedulers) of
        N when N > 0 ->
            system_task_failed_enqueue_1(Config);
        _ ->
            {skipped, "No dirty scheduler support"}
    end.

system_task_failed_enqueue_1(Config) ->
    Priv = proplists:get_value(priv_dir, Config),

    Purgers = [spawn_link(fun() -> purge_loop(Priv, Id) end)
               || Id <- lists:seq(1, erlang:system_info(schedulers))],
    Hogs = [spawn_link(fun() -> dirty_loop() end)
            || _ <- lists:seq(1, erlang:system_info(dirty_cpu_schedulers))],

    ct:sleep(5000),

    [begin
         unlink(Pid),
         exit(Pid, kill)
     end || Pid <- (Purgers ++ Hogs)],

    ok.

purge_loop(PrivDir, Id) ->
    Mod = "failed_enq_" ++ integer_to_list(Id),
    Path = PrivDir ++ "/" ++ Mod,
    file:write_file(Path ++ ".erl",
                    "-module('" ++ Mod ++ "').\n" ++
                        "-export([t/0]).\n" ++
                        "t() -> ok."),
    purge_loop_1(Path).
purge_loop_1(Path) ->
    {ok, Mod} = compile:file(Path, []),
    erlang:delete_module(Mod),
    erts_code_purger:purge(Mod),
    purge_loop_1(Path).

dirty_loop() ->
    ok = erts_debug:dirty_cpu(reschedule, 10000),
    dirty_loop().

gc_request_when_gc_disabled(Config) when is_list(Config) ->
    AIS = erts_debug:set_internal_state(available_internal_state, true),
    gc_request_when_gc_disabled_do(ref),
    gc_request_when_gc_disabled_do(immed),
    erts_debug:set_internal_state(available_internal_state, AIS).

gc_request_when_gc_disabled_do(ReqIdType) ->
    Master = self(),
    {P, M} = spawn_opt(fun () ->
			       true = erts_debug:set_internal_state(gc_state,
								    false),
			       Master ! {self(), gc_state, false},
			       receive after 1000 -> ok end,
			       Master ! {self(), gc_state, true},
			       false = erts_debug:set_internal_state(gc_state,
								     true),
			       receive after 100 -> ok end
		       end, [monitor, link]),
    receive {P, gc_state, false} -> ok end,
    ReqId = case ReqIdType of
                ref -> make_ref();
                immed -> immed
            end,
    async = garbage_collect(P, [{async, ReqId}]),
    receive
	{garbage_collect, ReqId, Result} ->
	    ct:fail({unexpected_gc, Result});
	{P, gc_state, true} ->
	    ok
    end,
    receive {garbage_collect, ReqId, true} -> ok end,
    receive {'DOWN', M, process, P, _Reason} -> ok end,
    ok.

gc_request_blast_when_gc_disabled(Config) when is_list(Config) ->
    Master = self(),
    AIS = erts_debug:set_internal_state(available_internal_state, true),
    {P, M} = spawn_opt(fun () ->
			       true = erts_debug:set_internal_state(gc_state,
								    false),
			       Master ! {self(), gc_state, false},
			       receive after 1000 -> ok end,
			       false = erts_debug:set_internal_state(gc_state,
								     true),
			       receive after 100 -> ok end
		       end, [monitor, link]),
    receive {P, gc_state, false} -> ok end,
    PMs = lists:map(fun (N) ->
			    Prio = case N rem 4 of
				       0 -> max;
				       1 -> high;
				       2 -> normal;
				       3 -> low
				   end,
			    spawn_opt(fun () ->
					      erlang:garbage_collect(P)
				      end, [monitor, link, {priority, Prio}])
		    end, lists:seq(1, 10000)),
    lists:foreach(fun ({Proc, Mon}) ->
			  receive
			      {'DOWN', Mon, process, Proc, normal} ->
				  ok
			  end
		  end,
		  PMs),
    erts_debug:set_internal_state(available_internal_state, AIS),
    receive {'DOWN', M, process, P, _Reason} -> ok end,
    ok.

otp_16436(Config) when is_list(Config) ->
    P = spawn_opt(fun () ->
                          erts_debug:dirty_io(wait, 1000)
                  end,
                  [{priority,high},link]),
    erlang:check_process_code(P, non_existing),
    unlink(P),
    exit(P, kill),
    ok.

otp_16642(Config) when is_list(Config) ->
    %%
    %% Whitebox testing...
    %%
    %% Ensure that low prio system tasks are interleaved with
    %% normal prio system tasks as they should.
    %%
    process_flag(priority, high),
    process_flag(scheduler, 1),
    Pid = spawn_opt(fun () -> receive after infinity -> ok end end,
                    [link, {scheduler, 1}]),
    ReqSTasks = fun (Prio, Start, Stop) ->
                        lists:foreach(
                          fun (N) ->
                                  erts_internal:request_system_task(
                                    Pid,
                                    Prio,
                                    {system_task_test,
                                     {Prio, N}})
                          end,
                          lists:seq(Start, Stop))
                end,
    MkResList = fun (Prio, Start, Stop) ->
                        lists:map(fun (N) ->
                                          {system_task_test,
                                           {Prio, N},
                                           true}
                                  end,
                                  lists:seq(Start, Stop))
                end,

    %%% Test when normal queue clears first...

    ReqSTasks(low, 0, 1),
    ReqSTasks(normal, 0, 10),
    ReqSTasks(low, 2, 4),
    ReqSTasks(normal, 11, 26),

    Msgs1 = recv_msgs(32),
    io:format("Got test 1 messages: ~p~n", [Msgs1]),

    ExpMsgs1 =
        MkResList(normal, 0, 7)
        ++ MkResList(low, 0, 0)
        ++ MkResList(normal, 8, 15)
        ++ MkResList(low, 1, 1)
        ++ MkResList(normal, 16, 23)
        ++ MkResList(low, 2, 2)
        ++ MkResList(normal, 24, 26)
        ++ MkResList(low, 3, 4),
    
    case Msgs1 =:= ExpMsgs1 of
        true ->
            ok;
        false ->
            io:format("Expected test 1 messages ~p~n",
                      [ExpMsgs1]),
            ct:fail(unexpected_messages)
    end,

    receive Unexp1 -> ct:fail({unexpected_message, Unexp1})
    after 500 -> ok
    end,

    io:format("Test 1 as expected~n", []),

    %%% Test when low queue clears first...

    ReqSTasks(low, 0, 1),
    ReqSTasks(normal, 0, 20),

    Msgs2 = recv_msgs(23),
    io:format("Got test 2 messages: ~p~n", [Msgs2]),

    ExpMsgs2 =
        MkResList(normal, 0, 7)
        ++ MkResList(low, 0, 0)
        ++ MkResList(normal, 8, 15)
        ++ MkResList(low, 1, 1)
        ++ MkResList(normal, 16, 20),
    
    case Msgs2 =:= ExpMsgs2 of
        true ->
            ok;
        false ->
            io:format("Expected test 2 messages ~p~n",
                      [ExpMsgs2]),
            ct:fail(unexpected_messages)
    end,

    receive Unexp2 -> ct:fail({unexpected_message, Unexp2})
    after 500 -> ok
    end,

    io:format("Test 2 as expected~n", []),

    unlink(Pid),
    exit(Pid, kill),
    false = is_process_alive(Pid),
    ok.

alias_bif(Config) when is_list(Config) ->
    alias_bif_test(node()),
    {ok, Peer, Node} = ?CT_PEER(),
    alias_bif_test(Node),
    stop_node(Peer, Node),
    ok.

alias_bif_test(Node) ->
    A1 = alias(),
    {P1, M1} = spawn_monitor(Node,
                             fun () ->
                                     A1 ! {A1, 1},
                                     A1 ! {A1, 2},
                                     [{A1, continue}] = recv_msgs(1),
                                     A1 ! {A1, 3},
                                     A1 ! {A1, 4}
                             end),
    [{A1,1},{A1,2}] = recv_msgs(2),
    unalias(A1),
    P1 ! {A1, continue},
    [{'DOWN', M1, _, _, _}] = recv_msgs(1),

    A2 = alias([explicit_unalias]),
    {P2, M2} = spawn_monitor(Node,
                             fun () ->
                                     A2 ! {A2, 1},
                                     A2 ! {A2, 2},
                                     [{A2, continue}] = recv_msgs(1),
                                     A2 ! {A2, 3},
                                     A2 ! {A2, 4}
                             end),
    [{A2,1},{A2,2}] = recv_msgs(2),
    unalias(A2),
    P2 ! {A2, continue},
    [{'DOWN', M2, _, _, _}] = recv_msgs(1),
    
    A3 = alias([reply]),
    {_P3, M3} = spawn_monitor(Node,
                              fun () ->
                                      A3 ! {A3, 1},
                                      A3 ! {A3, 2},
                                      A3 ! {A3, 3},
                                      A3 ! {A3, 4}
                              end),
    [{A3,1},{'DOWN', M3, _, _, _}] = recv_msgs(2),
    ok.

dist_frag_alias(Config) when is_list(Config) ->
    Tester = self(),
    {ok, Peer, Node} = ?CT_PEER(),
    {P,M} = spawn_monitor(Node,
                          fun () ->
                                  Alias = alias(),
                                  Tester ! {alias, Alias},
                                  receive
                                      {data, Data} ->
                                          garbage_collect(),
                                          Tester ! {received_data, Data}
                                  end,
                                  exit(end_of_test)
                          end),
    Data = term_to_binary(lists:seq(1, 1000000)),
    receive
        {alias, Alias} ->
            Alias ! {data, Data},
            receive
                {received_data, RecvData} ->
                    Data = RecvData;
                {'DOWN', M, process, P, R2} ->
                    ct:fail(R2)
            end;
        {'DOWN', M, process, P, R1} ->
            ct:fail(R1)
    end,
    receive
        {'DOWN', M, process, P, R3} ->
            end_of_test = R3
    end,
    peer:stop(Peer),
    ok.

dist_frag_unaliased(Config) when is_list(Config) ->
    %% Leak fixed by PR-7915 would have been detected using asan or valgrind
    %% when running this test...
    Tester = self(),
    {ok, Peer, Node} = ?CT_PEER(),
    {P,M} = spawn_monitor(Node,
                          fun () ->
                                  Alias = alias(),
                                  Tester ! {alias, Alias},
                                  receive
                                      {data, Data} ->
                                          garbage_collect(),
                                          unalias(Alias),
                                          Tester ! {received_data, Data},
                                          receive
                                              {data, _Data} ->
                                                  exit(received_data_again);
                                              end_of_test ->
                                                  exit(end_of_test)
                                          end
                                  end
                          end),
    Data = term_to_binary(lists:seq(1, 1000000)),
    receive
        {alias, Alias} ->
            Alias ! {data, Data},
            receive
                {received_data, RecvData} ->
                    Data = RecvData;
                {'DOWN', M, process, P, R2} ->
                    ct:fail(R2)
            end,
            Alias ! {data, Data},
            P ! end_of_test;
        {'DOWN', M, process, P, R1} ->
            ct:fail(R1)
    end,
    receive
        {'DOWN', M, process, P, R3} ->
            end_of_test = R3
    end,
    peer:stop(Peer),
    ok.

monitor_alias(Config) when is_list(Config) ->
    monitor_alias_test(node()),
    {ok, Peer, Node} = ?CT_PEER(),
    monitor_alias_test(Node),
    stop_node(Peer, Node),
    ok.

monitor_alias_test(Node) ->
    P1 = spawn(Node,
               fun () ->
                       [{alias, A1}] = recv_msgs(1),
                       A1 ! {A1, 1},
                       A1 ! {A1, 2},
                       [{A1, continue}] = recv_msgs(1),
                       A1 ! {A1, 3},
                       A1 ! {A1, 4}
               end),
    MA1 = monitor(process, P1, [{alias, explicit_unalias}]),
    P1 ! {alias, MA1},
    [{MA1,1},{MA1,2}] = recv_msgs(2),
    unalias(MA1),
    P1 ! {MA1, continue},
    [{'DOWN', MA1, _, _, _}] = recv_msgs(1),

    P2 = spawn(Node,
               fun () ->
                       [{alias, A2}] = recv_msgs(1),
                       A2 ! {A2, 1},
                       A2 ! {A2, 2},
                       [{A2, continue}] = recv_msgs(1),
                       A2 ! {A2, 3},
                       A2 ! {A2, 4}
               end),
    MA2 = monitor(process, P2, [{alias, demonitor}]),
    P2 ! {alias, MA2},
    [{MA2,1},{MA2,2}] = recv_msgs(2),
    demonitor(MA2),
    M2 = monitor(process, P2),
    P2 ! {MA2, continue},
    [{'DOWN', M2, _, _, _}] = recv_msgs(1),

    P3 = spawn(Node,
               fun () ->
                       [{alias, A3}] = recv_msgs(1),
                       A3 ! {A3, 1},
                       A3 ! {A3, 2}
               end),
    MA3 = monitor(process, P3, [{alias, demonitor}]),
    P3 ! {alias, MA3},
    [{MA3,1},{MA3,2},{'DOWN', MA3, _, _, _}] = recv_msgs(3),
    {_P3_1, M3_1} = spawn_monitor(Node,
                                  fun () ->
                                          MA3 ! {MA3, 3},
                                          MA3 ! {MA3, 4}
                                  end),
    [{'DOWN', M3_1, _, _, _}] = recv_msgs(1),
    
    P4 = spawn(Node,
               fun () ->
                       [{alias, _A4}] = recv_msgs(1)
               end),
    MA4 = monitor(process, P4, [{alias, reply_demonitor}]),
    P4 ! {alias, MA4},
    [{'DOWN', MA4, _, _, _}] = recv_msgs(1),
    {_P4_1, M4_1} = spawn_monitor(Node,
                                  fun () ->
                                          MA4 ! {MA4, 3},
                                          MA4 ! {MA4, 4}
                                  end),
    [{'DOWN', M4_1, _, _, _}] = recv_msgs(1),

    P5 = spawn(Node,
               fun () ->
                       [{alias, A5}] = recv_msgs(1),
                       A5 ! {A5, 1},
                       A5 ! {A5, 2}
               end),
    MA5 = monitor(process, P5, [{alias, reply_demonitor}]),
    M_5 = monitor(process, P5),
    P5 ! {alias, MA5},
    [{MA5,1},{'DOWN', M_5, _, _, _}] = recv_msgs(2),

    ok.
    

spawn_monitor_alias(Config) when is_list(Config) ->
    %% Exit signals with immediate exit reasons are sent
    %% in a different manner than compound exit reasons.
    spawn_monitor_alias_test(undefined, node(), spawn_opt, normal),
    spawn_monitor_alias_test(undefined, node(), spawn_opt, make_ref()),
    spawn_monitor_alias_test(undefined, node(), spawn_request, normal),
    spawn_monitor_alias_test(undefined, node(), spawn_request, make_ref()),
    {ok, Peer1, Node1} = ?CT_PEER(),
    spawn_monitor_alias_test(Peer1, Node1, spawn_opt, normal),
    {ok, Peer2, Node2} = ?CT_PEER(),
    spawn_monitor_alias_test(Peer2, Node2, spawn_opt, make_ref()),
    {ok, Peer3, Node3} = ?CT_PEER(),
    spawn_monitor_alias_test(Peer3, Node3, spawn_request, normal),
    {ok, Peer4, Node4} = ?CT_PEER(),
    spawn_monitor_alias_test(Peer4, Node4, spawn_request, make_ref()),
    ok.

spawn_monitor_alias_test(Peer, Node, SpawnType, ExitReason) ->
    Spawn = case SpawnType of
                spawn_opt ->
                    fun (F, O) ->
                            try
                                spawn_opt(Node, F, O)
                            catch
                                error:Err ->
                                    error({spawn_opt, Err})
                            end
                    end;
                spawn_request ->
                    fun (F, O) ->
                            try
                                ReqId = spawn_request(Node, F, O),
                                receive
                                    {spawn_reply, ReqId, ok, P} ->
                                        {P, ReqId};
                                    {spawn_reply, ReqId, error, Error} ->
                                        error(Error)
                                end
                            catch
                                error:Err ->
                                    error({spawn_request, Err})
                            end
                    end
            end,

    SpawnError = fun (OptList) ->
                         try
                             Spawn(fun () -> ok end, OptList),
                             error(ignored_error)
                         catch
                             error:{SpawnType, badarg} when SpawnType == spawn_opt ->
                                 ok;
                             error:{SpawnType, badopt} when SpawnType == spawn_request ->
                                 ok
                         end
                 end,

    SpawnError([{monitor, {{alias, explicit_unalias}}}]),
    SpawnError([{monitor, [{alias,alias}]}]),
    SpawnError([{monitor, [{aliases,explicit_unalias}]}]),
    SpawnError([{monitors, [{alias,explicit_unalias}]}]),
    
    {P1, MA1} = Spawn(fun () ->
                              [{alias, A1}] = recv_msgs(1),
                              A1 ! {A1, 1},
                              A1 ! {A1, 2},
                              [{A1, continue}] = recv_msgs(1),
                              A1 ! {A1, 3},
                              A1 ! {A1, 4},
                              exit(ExitReason)
                      end, [{monitor, [{alias,explicit_unalias}]}]),
    P1 ! {alias, MA1},
    [{MA1,1},{MA1,2}] = recv_msgs(2),
    unalias(MA1),
    P1 ! {MA1, continue},
    [{'DOWN', MA1, _, _, ExitReason}] = recv_msgs(1),

    {P2, MA2} = Spawn(fun () ->
                              [{alias, A2}] = recv_msgs(1),
                              A2 ! {A2, 1},
                              A2 ! {A2, 2},
                              [{A2, continue}] = recv_msgs(1),
                              A2 ! {A2, 3},
                              A2 ! {A2, 4},
                              exit(ExitReason)
                      end, [{monitor, [{alias, demonitor}]}]),
    P2 ! {alias, MA2},
    [{MA2,1},{MA2,2}] = recv_msgs(2),
    demonitor(MA2),
    M2 = monitor(process, P2),
    P2 ! {MA2, continue},
    [{'DOWN', M2, _, _, ExitReason}] = recv_msgs(1),

    {P3, MA3} = Spawn(fun () ->
                              [{alias, A3}] = recv_msgs(1),
                              A3 ! {A3, 1},
                              A3 ! {A3, 2},
                              exit(ExitReason)
                      end, [{monitor, [{alias, demonitor}]}]),
    P3 ! {alias, MA3},
    [{MA3,1},{MA3,2},{'DOWN', MA3, _, _, _}] = recv_msgs(3),
    {_P3_1, M3_1} = spawn_monitor(Node,
                                  fun () ->
                                          MA3 ! {MA3, 3},
                                          MA3 ! {MA3, 4},
                                          exit(ExitReason)
                                  end),
    [{'DOWN', M3_1, _, _, ExitReason}] = recv_msgs(1),
    
    {P4, MA4} = Spawn(fun () ->
                              [{alias, _A4}] = recv_msgs(1),
                              exit(ExitReason)
                      end, [{monitor, [{alias, reply_demonitor}]}]),
    P4 ! {alias, MA4},
    [{'DOWN', MA4, _, _, ExitReason}] = recv_msgs(1),
    {_P4_1, M4_1} = spawn_monitor(Node,
                                  fun () ->
                                          MA4 ! {MA4, 3},
                                          MA4 ! {MA4, 4},
                                          exit(ExitReason)
                                  end),
    [{'DOWN', M4_1, _, _, ExitReason}] = recv_msgs(1),

    {P5, MA5} = Spawn(fun () ->
                              [{alias, A5}] = recv_msgs(1),
                              A5 ! {A5, 1},
                              A5 ! {A5, 2},
                              exit(ExitReason)
                      end, [{monitor, [{alias, reply_demonitor}]}]),
    M_5 = monitor(process, P5),
    P5 ! {alias, MA5},
    [{MA5,1},{'DOWN', M_5, _, _, ExitReason}] = recv_msgs(2),

    case Node == node() of
        true ->
            ok;
        false ->
            {P6, MA6} = Spawn(fun () ->
                                      [{alias, A6}] = recv_msgs(1),
                                      A6 ! {A6, 1},
                                      A6 ! {A6, 2},
                                      receive after infinity -> ok end
                              end, [{monitor, [{alias, demonitor}]}]),
            P6 ! {alias, MA6},
            stop_node(Peer, Node),
            [{MA6,1},{MA6,2},{'DOWN', MA6, _, _, noconnection}] = recv_msgs(3),
            {_P6_1, M6_1} = spawn_monitor(fun () ->
                                                  MA6 ! {MA6, 3},
                                                  MA6 ! {MA6, 4}
                                          end),
            [{'DOWN', M6_1, _, _, _}] = recv_msgs(1),
    
            ok
    end.

demonitor_aliasmonitor(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    Fun = fun () ->
                  receive
                      {alias, Alias} ->
                          Alias ! {alias_reply, Alias, self()}
                  end
          end,
    LPid = spawn(Fun),
    RPid = spawn(Node, Fun),
    AliasMonitor = erlang:monitor(process, LPid, [{alias, explicit_unalias}]),
    erlang:demonitor(AliasMonitor),
    LPid ! {alias, AliasMonitor},
    receive {alias_reply, AliasMonitor, LPid} -> ok end,
    %% Demonitor signal has been received and cleaned up. Cleanup of
    %% it erroneously removed it from the alias table which caused
    %% remote use of the alias to stop working...
    RPid ! {alias, AliasMonitor},
    receive {alias_reply, AliasMonitor, RPid} -> ok end,
    exit(LPid, kill),
    peer:stop(Peer),
    false = is_process_alive(LPid),
    ok.

down_aliasmonitor(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    LPid = spawn(fun () -> receive infinty -> ok end end),
    RPid = spawn(Node,
                 fun () ->
                         receive
                             {alias, Alias} ->
                                 Alias ! {alias_reply, Alias, self()}
                         end
                 end),
    AliasMonitor = erlang:monitor(process, LPid, [{alias, explicit_unalias}]),
    exit(LPid, bye),
    receive {'DOWN', AliasMonitor, process, LPid, bye} -> ok end,
    %% Down signal has been received and cleaned up. Cleanup of
    %% it erroneously removed it from the alias table which caused
    %% remote use of the alias to stop working...
    RPid ! {alias, AliasMonitor},
    receive {alias_reply, AliasMonitor, RPid} -> ok end,
    peer:stop(Peer),
    ok.

monitor_tag(Config) when is_list(Config) ->
    %% Exit signals with immediate exit reasons are sent
    %% in a different manner than compound exit reasons, and
    %% immediate tags are stored in a different manner than
    %% compound tags.
    monitor_tag_test(undefined, node(), spawn_opt, immed, normal),
    monitor_tag_test(undefined, node(), spawn_opt, make_ref(), normal),
    monitor_tag_test(undefined, node(), spawn_opt, immed, make_ref()),
    monitor_tag_test(undefined, node(), spawn_opt, make_ref(), make_ref()),
    monitor_tag_test(undefined, node(), spawn_request, immed, normal),
    monitor_tag_test(undefined, node(), spawn_request, make_ref(), normal),
    monitor_tag_test(undefined, node(), spawn_request, immed, make_ref()),
    monitor_tag_test(undefined, node(), spawn_request, make_ref(), make_ref()),
    {ok, Peer1, Node1} = ?CT_PEER(),
    monitor_tag_test(Peer1, Node1, spawn_opt, immed, normal),
    {ok, Peer2, Node2} = ?CT_PEER(),
    monitor_tag_test(Peer2, Node2, spawn_opt, make_ref(), normal),
    {ok, Peer3, Node3} = ?CT_PEER(),
    monitor_tag_test(Peer3, Node3, spawn_opt, immed, make_ref()),
    {ok, Peer4, Node4} = ?CT_PEER(),
    monitor_tag_test(Peer4, Node4, spawn_opt, make_ref(), make_ref()),
    {ok, Peer5, Node5} = ?CT_PEER(),
    monitor_tag_test(Peer5, Node5, spawn_request, immed, normal),
    {ok, Peer6, Node6} = ?CT_PEER(),
    monitor_tag_test(Peer6, Node6, spawn_request, make_ref(), normal),
    {ok, Peer7, Node7} = ?CT_PEER(),
    monitor_tag_test(Peer7, Node7, spawn_request, immed, make_ref()),
    {ok, Peer8, Node8} = ?CT_PEER(),
    monitor_tag_test(Peer8, Node8, spawn_request, make_ref(), make_ref()),
    ok.

monitor_tag_test(Peer, Node, SpawnType, Tag, ExitReason) ->

    P1 = spawn(Node, fun () -> receive go -> ok end, exit(ExitReason) end),
    M1 = monitor(process, P1, [{tag, Tag}]),
    P1 ! go,
    [{Tag, M1, process, P1, ExitReason}] = recv_msgs(1),

    M1_2 = monitor(process, P1, [{tag, Tag}]),
    [{Tag, M1_2, process, P1, noproc}] = recv_msgs(1),

    Spawn = case SpawnType of
                spawn_opt ->
                    fun (F, O) ->
                            try
                                spawn_opt(Node, F, O)
                            catch
                                error:Err ->
                                    error({spawn_opt, Err})
                            end
                    end;
                spawn_request ->
                    fun (F, O) ->
                            try
                                ReqId = spawn_request(Node, F, O),
                                receive
                                    {spawn_reply, ReqId, ok, P} ->
                                        {P, ReqId};
                                    {spawn_reply, ReqId, error, Error} ->
                                        error(Error)
                                end
                            catch
                                error:Err ->
                                    error({spawn_request, Err})
                            end
                    end
            end,

    {P2, M2} = Spawn(fun () -> exit(ExitReason) end, [{monitor, [{tag, Tag}]}]),
    [{Tag, M2, process, P2, ExitReason}] = recv_msgs(1),

    case Node == node() of
        true ->
            ok;
        false ->
            {P3, M3} = Spawn(fun () -> receive after infinity -> ok end end,
                             [{monitor, [{tag, Tag}]}]),
            stop_node(Peer, Node),
            [{Tag, M3, process, P3, noconnection}] = recv_msgs(1),

            case SpawnType == spawn_opt of
                true ->
                    {P6, M6} = Spawn(fun () -> receive after infinity -> ok end end,
                                     [{monitor, [{tag, Tag}]}]),
                    [{Tag, M6, process, P6, noconnection}] = recv_msgs(1);
                false ->
                    ok
            end,
            ok
    end.

no_pid_wrap(Config) when is_list(Config) ->
    process_flag(priority, high),
    SOnln = erlang:system_info(schedulers_online),
    Pid = spawn(fun () -> ok end),
    exit(Pid, kill),
    false = is_process_alive(Pid),
    ChkSpwndPid = fun () ->
                          check_spawned_pid(Pid)
                  end,
    MPs = maps:from_list(lists:map(fun (_) ->
                                           {P, M} = spawn_monitor(ChkSpwndPid),
                                           {M, P}
                                   end, lists:seq(1, SOnln))),
    Res = receive
              {'DOWN', M, process, _, pid_reused} when is_map_key(M, MPs) ->
                  case erlang:system_info(wordsize) of
                      8 ->
                          ct:fail("Process identifier reused"),
                          error;
                      4 ->
                          {comment,
                           "Process identifer reused, but this is"
                           ++ "expected since this is a 32-bit system"}
                  end;
              {'DOWN', _, _, _, _} = Down ->
                  ct:fail({unexpected_down, Down}),
                  error
          after
              3*60*1000 ->
                  ok
          end,
    maps:foreach(fun (_, P) ->
                         exit(P, kill)
                 end, MPs),
    maps:foreach(fun (_, P) ->
                         false = is_process_alive(P)
                 end, MPs),
    Res.

check_spawned_pid(OldPid) ->
    Pid = spawn(fun () -> ok end),
    case OldPid == Pid of
        false ->
            check_spawned_pid(OldPid);
        true ->
            exit(pid_reused)
    end.

%% Internal functions

recv_msgs(N) ->
    recv_msgs(N, []).

recv_msgs(0, Msgs) ->
    lists:reverse(Msgs);
recv_msgs(N, Msgs) ->
    receive
        Msg ->
            recv_msgs(N-1, [Msg|Msgs])
    end.

wait_until(Fun) ->
    case Fun() of
	true -> true;
	false -> receive after 10 -> wait_until(Fun) end
    end.

tok_loop() ->
    tok_loop(hej).

tok_loop(hej) ->
    tok_loop(hopp);
tok_loop(hopp) ->
    tok_loop(hej).

id(I) -> I.

stop_node(Peer, Node) ->
    verify_nc(node()),
    verify_nc(Node),
    peer:stop(Peer).

verify_nc(Node) ->
    P = self(),
    Ref = make_ref(),
    Pid = spawn(Node,
                fun() ->
                        R = erts_test_utils:check_node_dist(fun(E) -> E end),
                        P ! {Ref, R}
                end),
    MonRef = monitor(process, Pid),
    receive
        {Ref, ok} ->
            demonitor(MonRef,[flush]),
            ok;
        {Ref, Error} ->
            ct:log("~s",[Error]),
            ct:fail(failed_nc_refc_check);
        {'DOWN', MonRef, _, _, _} = Down ->
            ct:log("~p",[Down]),
            ct:fail(crashed_nc_refc_check)
    end.

enable_internal_state() ->
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> true;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end.

sys_mem_cond_run(OrigReqSizeMB, TestFun) when is_integer(OrigReqSizeMB) ->
    %% Debug normally needs more memory, so double the requirement
    Debug = erlang:system_info(debug_compiled),
    ReqSizeMB = if Debug -> OrigReqSizeMB * 2; true -> OrigReqSizeMB end,
    case total_memory() of
	TotMem when is_integer(TotMem), TotMem >= ReqSizeMB ->
	    TestFun();
	TotMem when is_integer(TotMem) ->
	    {skipped, "Not enough memory ("++integer_to_list(TotMem)++" MB)"};
	undefined ->
	    {skipped, "Could not retrieve memory information"}
    end.


total_memory() ->
    %% Total memory in MB.
    try
	SMD = memsup:get_system_memory_data(),
        TM = proplists:get_value(
               available_memory, SMD,
               proplists:get_value(
                 total_memory, SMD,
                 proplists:get_value(
                   system_total_memory, SMD))),
        TM div (1024*1024)
    catch
	_ : _ ->
	    undefined
    end.

start_ei_node(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    FwdNodeExe = filename:join(DataDir, "fwd_node"),
    Name = atom_to_list(?MODULE)
        ++ "-" ++ "ei_node"
        ++ "-" ++ integer_to_list(erlang:system_time(second))
        ++ "-" ++ integer_to_list(erlang:unique_integer([positive])),
    Cookie = atom_to_list(erlang:get_cookie()),
    HostName = get_hostname(),
    Node = list_to_atom(Name++"@"++HostName),
    Creation = integer_to_list(rand:uniform((1 bsl 15) - 4) + 3),
    Parent = self(),
    Pid = spawn_link(fun () ->
                             register(cnode_forward_receiver, self()),
                             process_flag(trap_exit, true),
                             Args = ["-sname", Name,
                                     "-cookie", Cookie,
                                     "-creation", Creation],
                             io:format("Starting ei_node: ~p ~p~n",
                                       [FwdNodeExe, Args]),
                             Port = erlang:open_port({spawn_executable, FwdNodeExe},
                                                     [use_stdio, {args, Args}]),
                             receive
                                 {Port, {data, "accepting"}} -> ok
                             end,
                             ei_node_handler_loop(Node, Parent, Port)
                     end),
    put({ei_node_handler, Node}, Pid),
    case check_ei_node(Node) of
        ok -> {ok, Node};
        Error -> Error
    end.

check_ei_node(Node) ->
    Key = {ei_node_handler, Node},
    case get(Key) of
        undefined ->
            {error, no_handler};
        Pid when is_pid(Pid) ->
            Pid ! {check_node, self()},
            receive
                {check_node, Pid, Res} ->
                    Res
            after 3000 ->
                    {error, no_handler_response}
            end
    end.

stop_ei_node(Node) ->
    case check_ei_node(Node) of
        ok ->
            Key = {ei_node_handler, Node},
            case get(Key) of
                undefined ->
                    {error, no_handler};
                Pid when is_pid(Pid) ->
                    Pid ! {stop_node, self()},
                    receive
                        {stop_node, Pid} ->
                            put(Key, undefined),
                            ok
                    after 2000 ->
                            {error, no_handler_response}
                    end
            end;
        Error ->
            Error
    end.

ei_node_handler_loop(Node, Parent, Port) ->
    receive
        {'EXIT', Parent, Reason} ->
            erlang:disconnect_node(Node),
            (catch port_close(Port)),
            exit(Reason);
        {stop_node, Parent} ->
            erlang:disconnect_node(Node),
            (catch port_close(Port)),
            Parent ! {stop_node, self()},
            exit(normal);
        {check_node, Parent} ->
            Ref = make_ref(),
            {a_name, Node} ! Ref,
            receive
                Ref ->
                    Parent ! {check_node, self(), ok}
            after
                2000 ->
                    Parent ! {check_node, self(), {error, no_node_response}}
            end;
        Msg ->
            Msgs = fetch_all_messages([Msg]),
            erlang:disconnect_node(Node),
            (catch port_close(Port)),
            exit({ei_node_handler, Node, unexpected_messages, Msgs})
    end,
    ei_node_handler_loop(Node, Parent, Port).

fetch_all_messages(Msgs) ->
    receive
        Msg ->
            fetch_all_messages([Msg|Msgs])
    after
        0 ->
            Msgs
    end.

get_hostname() ->
    get_hostname(atom_to_list(node())).

get_hostname([$@ | HostName]) ->
    HostName;
get_hostname([_ | Rest]) ->
    get_hostname(Rest).

receive_any() ->
    receive M -> M end.
