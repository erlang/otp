%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
%%	register/2 (partially)

-include_lib("common_test/include/ct.hrl").

-define(heap_binary_size, 64).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, spawn_with_binaries/1,
	 t_exit_1/1, t_exit_2_other/1, t_exit_2_other_normal/1,
	 self_exit/1, normal_suicide_exit/1, abnormal_suicide_exit/1,
	 t_exit_2_catch/1, trap_exit_badarg/1, trap_exit_badarg_in_bif/1,
	 exit_and_timeout/1, exit_twice/1,
	 t_process_info/1, process_info_other/1, process_info_other_msg/1,
	 process_info_other_dist_msg/1,
	 process_info_2_list/1, process_info_lock_reschedule/1,
	 process_info_lock_reschedule2/1,
	 process_info_lock_reschedule3/1,
         process_info_garbage_collection/1,
         process_info_smoke_all/1,
         process_info_status_handled_signal/1,
         process_info_reductions/1,
	 bump_reductions/1, low_prio/1, binary_owner/1, yield/1, yield2/1,
	 otp_4725/1, bad_register/1, garbage_collect/1, otp_6237/1,
	 process_info_messages/1, process_flag_badarg/1, process_flag_heap_size/1,
	 spawn_opt_heap_size/1, spawn_opt_max_heap_size/1,
	 processes_large_tab/1, processes_default_tab/1, processes_small_tab/1,
	 processes_this_tab/1, processes_apply_trap/1,
	 processes_last_call_trap/1, processes_gc_trap/1,
	 processes_term_proc_list/1,
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
         otp_16436/1]).
-export([prio_server/2, prio_client/2, init/1, handle_event/2]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([hangaround/2, processes_bif_test/0, do_processes/1,
	 processes_term_proc_list_test/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 9}}].

all() -> 
    [spawn_with_binaries, t_exit_1, {group, t_exit_2},
     trap_exit_badarg, trap_exit_badarg_in_bif,
     t_process_info, process_info_other, process_info_other_msg,
     process_info_other_dist_msg, process_info_2_list,
     process_info_lock_reschedule,
     process_info_lock_reschedule2,
     process_info_lock_reschedule3,
     process_info_garbage_collection,
     process_info_smoke_all,
     process_info_status_handled_signal,
     process_info_reductions,
     bump_reductions, low_prio, yield, yield2, otp_4725,
     bad_register, garbage_collect, process_info_messages,
     process_flag_badarg, process_flag_heap_size,
     spawn_opt_heap_size, spawn_opt_max_heap_size, otp_6237,
     {group, processes_bif},
     {group, otp_7738}, garb_other_running,
     {group, system_task}].

groups() -> 
    [{t_exit_2, [],
      [t_exit_2_other, t_exit_2_other_normal, self_exit,
       normal_suicide_exit, abnormal_suicide_exit,
       t_exit_2_catch, exit_and_timeout, exit_twice]},
     {processes_bif, [],
      [processes_large_tab, processes_default_tab,
       processes_small_tab, processes_this_tab,
       processes_last_call_trap, processes_apply_trap,
       processes_gc_trap, processes_term_proc_list]},
     {otp_7738, [],
      [otp_7738_waiting, otp_7738_suspended,
       otp_7738_resume]},
     {system_task, [],
      [no_priority_inversion, no_priority_inversion2,
       system_task_blast, system_task_on_suspended, system_task_failed_enqueue,
       otp_16436]}].

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
    case erlang:system_info(debug_compiled) of
        true ->
            {skip, "Don't run in debug"};
        false ->
            [{testcase, Func} | Config]
    end;
init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    %% Restore max_heap_size to default value.
    erlang:system_flag(max_heap_size,
                       #{size => 0,
                         kill => true,
                         error_logger => true}),
    ok.

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

%% Tests that exit(self(), die) cannot be catched.
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
    test_server:do_times(10, fun trap_exit_badarg/0),
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
    {max_heap_size, #{ size := 0, kill := true, error_logger := true}} =
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
    {max_heap_size, #{ size := 0, kill := true, error_logger := true}} =
        process_info(self(), max_heap_size),

    Pid ! stop,
    ok.

process_info_other_dist_msg(Config) when is_list(Config) ->
    %%
    %% Check that process_info can handle messages that have not been
    %% decoded yet.
    %%
    {ok, Node} = start_node(Config),
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
    stop_node(Node),
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
                    fullsweep_after],

    {ok, Node} = start_node(Config, ""),
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
    stop_node(Node),
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
    {S1, S2} = case erlang:system_info(schedulers) of
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
	     ++ ") is enabled. Testcase gets messed up by modfied "
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
                                                        kill => true,
                                                        error_logger => gurka }) end),
    chk_badarg(fun () -> process_flag(max_heap_size, #{ size => 1 bsl 64 }) end),

    chk_badarg(fun () -> process_flag(priority, 4711) end),
    chk_badarg(fun () -> process_flag(save_calls, hmmm) end),
    P= spawn_link(fun () -> receive die -> ok end end),
    chk_badarg(fun () -> process_flag(P, save_calls, hmmm) end),
    chk_badarg(fun () -> process_flag(gurka, save_calls, hmmm) end),
    P ! die,
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
    sys_mem_cond_run(2048, fun () -> processes_large_tab_test(Config) end).

processes_large_tab_test(Config) ->
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
    ProcTabSize2 = case erlang:system_info(debug_compiled) of
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
    {ok, LargeNode} = start_node(Config,
				       "+P " ++ integer_to_list(ProcTabSize)),
    Res = rpc:call(LargeNode, ?MODULE, processes_bif_test, []),
    case rpc:call(LargeNode,
			erts_debug,
			get_internal_state,
			[processes_bif_info]) of
	      #ptab_list_bif_info{tab_chunks = Chunks} when is_integer(Chunks),
							    Chunks > 1 -> ok;
	      PBInfo -> ct:fail(PBInfo)
	  end,
    stop_node(LargeNode),
    chk_processes_bif_test_res(Res).

processes_default_tab(Config) when is_list(Config) ->
    sys_mem_cond_run(1024, fun () -> processes_default_tab_test(Config) end).

processes_default_tab_test(Config) ->
    {ok, DefaultNode} = start_node(Config, ""),
    Res = rpc:call(DefaultNode, ?MODULE, processes_bif_test, []),
    stop_node(DefaultNode),
    chk_processes_bif_test_res(Res).

processes_small_tab(Config) when is_list(Config) ->
    {ok, SmallNode} = start_node(Config, "+P 1024"),
    Res    = rpc:call(SmallNode, ?MODULE, processes_bif_test, []),
    PBInfo = rpc:call(SmallNode, erts_debug, get_internal_state, [processes_bif_info]),
    stop_node(SmallNode),
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

    ok = do_processes_bif_test(WantReds, WillTrap, Processes),

    case WillTrap of
	false ->
	    ok;
	true ->
	    %% Do it again with a process suspended while
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

    %% Test that numerical limit works
    max_heap_size_test(1024, 1024, true, true),

    %% Test that map limit works
    max_heap_size_test(#{ size => 1024 }, 1024, true, true),

    %% Test that no kill is sent
    max_heap_size_test(#{ size => 1024, kill => false }, 1024, false, true),

    %% Test that no error_logger report is sent
    max_heap_size_test(#{ size => 1024, error_logger => false }, 1024, true, false),

    %% Test that system_flag works
    erlang:system_flag(max_heap_size, #{ size => 0, kill => false,
                                         error_logger => true}),
    max_heap_size_test(#{ size => 1024 }, 1024, false, true),
    max_heap_size_test(#{ size => 1024, kill => true }, 1024, true, true),

    erlang:system_flag(max_heap_size, #{ size => 0, kill => true,
                                         error_logger => false}),
    max_heap_size_test(#{ size => 1024 }, 1024, true, false),
    max_heap_size_test(#{ size => 1024, error_logger => true }, 1024, true, true),

    erlang:system_flag(max_heap_size, #{ size => 1 bsl 20, kill => true,
                                         error_logger => true}),
    max_heap_size_test(#{ }, 1 bsl 20, true, true),

    erlang:system_flag(max_heap_size, #{ size => 0, kill => true,
                                         error_logger => true}),

    %% Test that ordinary case works as expected again
    max_heap_size_test(1024, 1024, true, true),

    ok.

max_heap_size_test(Option, Size, Kill, ErrorLogger)
  when map_size(Option) == 0 ->
    max_heap_size_test([], Size, Kill, ErrorLogger);
max_heap_size_test(Option, Size, Kill, ErrorLogger)
  when is_map(Option); is_integer(Option) ->
    max_heap_size_test([{max_heap_size, Option}], Size, Kill, ErrorLogger);
max_heap_size_test(Option, Size, Kill, ErrorLogger) ->
    OomFun = fun F() -> timer:sleep(5),[lists:seq(1,1000)|F()] end,
    Pid = spawn_opt(OomFun, Option),
    {max_heap_size, MHSz} = erlang:process_info(Pid, max_heap_size),
    ct:log("Default: ~p~nOption: ~p~nProc: ~p~n",
           [erlang:system_info(max_heap_size), Option, MHSz]),

    #{ size := Size} = MHSz,

    Ref = erlang:monitor(process, Pid),
    if Kill ->
            receive
                {'DOWN', Ref, process, Pid, killed} ->
                    ok
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

processes_term_proc_list(Config) when is_list(Config) ->
    Tester = self(),

    Run = fun(Args) ->
              {ok, Node} = start_node(Config, Args),
              RT = spawn_link(Node, fun () ->
                              receive after 1000 -> ok end,
                              as_expected = processes_term_proc_list_test(false),
                              Tester ! {it_worked, self()}
                      end),
              receive {it_worked, RT} -> ok end,
              stop_node(Node)
          end,

    %% We have to run this test case with +S1 since instrument:allocations()
    %% will report a free()'d block as present until it's actually deallocated
    %% by its employer.
    Run("+MSe true +Muatags false +S1"),
    Run("+MSe true +Muatags true +S1"),

    ok.

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
    Reds = lists:foldl(fun (_, OldReds) ->
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
    RL = request_gc(PL, low),
    RN = request_gc(PL, normal),
    RH = request_gc(PL, high),
    receive
	{garbage_collect, _, _} ->
	    ct:fail(unexpected_gc)
    after 1000 ->
	    ok
    end,
    RM = request_gc(PL, max),
    receive
	{garbage_collect, RM, true} ->
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
	{garbage_collect, RH, true} ->
	    ok
    end,
    receive
	{garbage_collect, RN, true} ->
	    ok
    end,
    receive
	{garbage_collect, RL, true} ->
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

request_gc(Pid, Prio) ->
    Ref = make_ref(),
    erts_internal:request_system_task(Pid, Prio, {garbage_collect, Ref, major}),
    Ref.

system_task_blast(Config) when is_list(Config) ->
    Me = self(),
    GCReq = fun () ->
		    RL = gc_req(Me, 100),
		    lists:foreach(fun (R) ->
					  receive
					      {garbage_collect, R, true} ->
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

gc_req(_Pid, 0) ->
    [];
gc_req(Pid, N) ->
    R0 = request_gc(Pid, low),
    R1 = request_gc(Pid, normal),
    R2 = request_gc(Pid, high),
    R3 = request_gc(Pid, max),
    [R0, R1, R2, R3 | gc_req(Pid, N-1)].

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

%% Internal functions

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
    %% Totat memory in MB.
    try
	MemoryData = memsup:get_system_memory_data(),
	case lists:keysearch(total_memory, 1, MemoryData) of
	    {value, {total_memory, TM}} ->
		TM div (1024*1024);
	    false ->
		{value, {system_total_memory, STM}} =
		    lists:keysearch(system_total_memory, 1, MemoryData),
		STM div (1024*1024)
	end
    catch
	_ : _ ->
	    undefined
    end.
