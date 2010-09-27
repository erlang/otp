%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

-module(hibernate_SUITE).

-include("test_server.hrl").

-export([all/1,init_per_testcase/2,fin_per_testcase/2,
	 basic/1,dynamic_call/1,min_heap_size/1,bad_args/1,
	 messages_in_queue/1,undefined_mfa/1, no_heap/1]).

%% Used by test cases.
-export([basic_hibernator/1,dynamic_call_hibernator/2,messages_in_queue_restart/2, no_heap_loop/0]).

all(suite) ->
    [basic,dynamic_call,min_heap_size,bad_args,messages_in_queue,undefined_mfa,no_heap].

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

%%%
%%% Testing the basic functionality of erlang:hibernate/3.
%%%

basic(Config) when is_list(Config) ->
    Ref = make_ref(),
    Info = {self(),Ref},
    ExpectedHeapSz = case erlang:system_info(heap_type) of
			 private -> erts_debug:size([Info]);
			 hybrid -> erts_debug:size([a|b])
		     end,
    ?line Child = spawn_link(fun() -> basic_hibernator(Info) end),
    ?line hibernate_wake_up(100, ExpectedHeapSz, Child),
    ?line Child ! please_quit_now,
    ok.

hibernate_wake_up(0, _, _) -> ok;
hibernate_wake_up(N, ExpectedHeapSz, Child) ->
    {heap_size,Before} = process_info(Child, heap_size),
    case N rem 2 of
	0 ->
	    Child ! {acquire_old_heap,self()},
	    receive
		done -> ok
	    end;
	1 -> ok
    end,
    ?line Child ! {hibernate,self()},
    ?line wait_until(fun () ->
			     {current_function,{erlang,hibernate,3}} ==
				 process_info(Child, current_function)
		     end),
    ?line {message_queue_len,0} = process_info(Child, message_queue_len),
    ?line {status,waiting} = process_info(Child, status),
    ?line {heap_size,ExpectedHeapSz} = process_info(Child, heap_size),
    io:format("Before hibernation: ~p  After hibernation: ~p\n",
	      [Before,ExpectedHeapSz]),
    ?line Child ! {whats_up,self()},
    ?line receive
	      {all_fine,X,Child,_Ref} ->
		  if
		      N =:= 1 -> io:format("~p\n", [X]);
		      true -> ok
		  end,
		  {backtrace,Bin} = process_info(Child, backtrace),
		  if
		      size(Bin) > 1000 ->
			  io:format("~s\n", [binary_to_list(Bin)]),
			  ?line ?t:fail(stack_is_growing);
		      true ->
			  hibernate_wake_up(N-1, ExpectedHeapSz, Child)
		  end;
	      Other ->
		  ?line io:format("~p\n", [Other]),
		  ?line ?t:fail(unexpected_message)
	  end.

basic_hibernator(Info) ->
    {catchlevel,0} = process_info(self(), catchlevel),
    receive
	Any ->
	    basic_hibernator_msg(Any, Info),
	    basic_hibernator(Info)
    end.

basic_hibernator_msg({hibernate,_}, Info) ->
    catch erlang:hibernate(?MODULE, basic_hibernator, [Info]),
    exit(hibernate_returned);
basic_hibernator_msg({acquire_old_heap,Parent}, _) ->
    acquire_old_heap(),
    Parent ! done;
basic_hibernator_msg({whats_up,Parent}, {Parent,Ref}) ->
    {heap_size,HeapSize} = process_info(self(), heap_size),
    io:format("Heap size after waking up: ~p\n", [HeapSize]),
    X = whats_up_calc(5000, 2, math:pi(), 4, 5, 6, 7, 8.5, 9, []),
    Parent ! {all_fine,X,self(),Ref};
basic_hibernator_msg(please_quit_now, _) ->
    exit(normal);
basic_hibernator_msg(Other, _) ->
    exit({unexpected,Other}).

acquire_old_heap() ->
    case process_info(self(), [heap_size,total_heap_size]) of
	[{heap_size,Sz},{total_heap_size,Total}] when Sz < Total ->
	    ok;
	_ ->
	    acquire_old_heap()
    end.

%% The point with this calculation is to force memory to be
%% allocated for the argument registers in the process structure.
%% The allocation will be forced if the process is scheduled out
%% while calling a function with more than 6 arguments.
whats_up_calc(0, A2, A3, A4, A5, A6, A7, A8, A9, Acc) ->
    {Acc,A2+A3+A4+A5+A6+A7+A8+A9};
whats_up_calc(A1, A2, A3, A4, A5, A6, A7, A8, A9, Acc) ->
    whats_up_calc(A1-1, A2+1, A3+2, A4+3, A5+4, A6+5, A7+6, A8+7, A9+8, [A1,A2|Acc]).

%%%
%%% Testing a call to erlang:hibernate/3 that the compiler and loader do not
%%% translate to an instruction.
%%%

dynamic_call(Config) when is_list(Config) ->
    Ref = make_ref(),
    Info = {self(),Ref},
    ExpectedHeapSz = case erlang:system_info(heap_type) of
			 private -> erts_debug:size([Info]);
			 hybrid -> erts_debug:size([a|b])
		     end,
    ?line Child = spawn_link(fun() -> ?MODULE:dynamic_call_hibernator(Info, hibernate) end),
    ?line hibernate_wake_up(100, ExpectedHeapSz, Child),
    ?line Child ! please_quit_now,
    ok.

dynamic_call_hibernator(Info, Function) ->
    {catchlevel,0} = process_info(self(), catchlevel),
    receive
	Any ->
	    dynamic_call_hibernator_msg(Any, Function, Info),
	    dynamic_call_hibernator(Info, Function)
    end.

dynamic_call_hibernator_msg({hibernate,_}, Function, Info) ->
    catch apply(erlang, Function, [?MODULE, basic_hibernator, [Info]]),
    exit(hibernate_returned);
dynamic_call_hibernator_msg(Msg, _Function, Info) ->
    basic_hibernator_msg(Msg, Info).

%%%
%%% Testing setting the minimum heap size.
%%%

min_heap_size(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) of
	true -> {skip, "Test case relies on trace which is not available in HiPE"};
	false -> min_heap_size_1(Config)
    end.

min_heap_size_1(Config) when is_list(Config) ->
    ?line erlang:trace(new, true, [call]),
    MFA = {?MODULE,min_hibernator,1},
    ?line 1 = erlang:trace_pattern(MFA, true, [local]),
    Ref = make_ref(),
    Info = {self(),Ref},
    ?line Child = spawn_opt(fun() -> min_hibernator(Info) end,
			    [{min_heap_size,15000},link]),
    receive
	{trace,Child,call,{?MODULE,min_hibernator,_}} ->
	    ?line 1 = erlang:trace_pattern(MFA, false, [local]),
	    ?line erlang:trace(new, false, [call])
    end,
    {heap_size,HeapSz} = process_info(Child, heap_size),
    io:format("Heap size: ~p\n", [HeapSz]),
    ?line if
	      HeapSz < 20 -> ok
	  end,
    ?line Child ! wake_up,
    receive
	{heap_size,AfterSize} ->
	    io:format("Heap size after wakeup: ~p\n", [AfterSize]),
	    ?line
		if
		    AfterSize >= 15000 -> ok
		end;
	Other ->
	    io:format("Unexpected: ~p\n", [Other]),
	    ?line ?t:fail()
    end.

min_hibernator({Parent,_Ref}) ->
    erlang:hibernate(erlang, apply, [fun min_hibernator_recv/1, [Parent]]).

min_hibernator_recv(Parent) ->
    receive
	wake_up ->
	    Parent ! process_info(self(), heap_size)
    end.

%%%
%%% Testing feeding erlang:hibernate/3 with bad arguments.
%%%

bad_args(Config) when is_list(Config) ->
    ?line bad_args(?MODULE, {name,glurf}, [0]),
    ?line {'EXIT',{system_limit,_}} = 
	(catch erlang:hibernate(x, y, lists:duplicate(5122, xxx))),
    ?line bad_args(42, name, [0]),
    ?line bad_args(xx, 42, [1]),
    ?line bad_args(xx, 42, glurf),
    ?line bad_args(xx, 42, {}),
    ?line bad_args({}, name, [2]),
    ?line bad_args({1}, name,  [3]),
    ?line bad_args({1,2,3}, name, [4]),
    ?line bad_args({1,2,3}, name, [5]),
    ?line bad_args({1,2,3,4}, name, [6]),
    ?line bad_args({1,2,3,4,5,6}, name,[7]),
    ?line bad_args({1,2,3,4,5}, name, [8]),
    ?line bad_args({1,2}, name, [9]),
    ?line bad_args([1,2], name, [9]),
    ?line bad_args(55.0, name, [9]),
    ok.

bad_args(Mod, Name, Args) ->
    Res = (catch erlang:hibernate(Mod, Name, Args)),
    erlang:garbage_collect(),
    case Res of
	{'EXIT',{badarg,_Where}} ->
	    io:format("erlang:hibernate(~p, ~p, ~p) -> ~p\n", [Mod,Name,Args,Res]);
	Other ->
	    io:format("erlang:hibernate(~p, ~p, ~p) -> ~p\n", [Mod,Name,Args,Res]),
	    ?t:fail({bad_result,Other})
    end.


%%%
%%% Testing calling erlang:hibernate/3 with messages already in the message queue.
%%%

messages_in_queue(Config) when is_list(Config) ->
    Self = self(),
    Msg = {Self,make_ref(),a,message},
    Pid = spawn_link(fun() -> messages_in_queue_1(Self, Msg) end),
    Pid ! Msg,
    Pid ! go_ahead,
    receive
	done -> ok;
	Other ->
	    ?line io:format("~p\n", [Other]),
	    ?line ?t:fail(unexpected_message)
    end.

messages_in_queue_1(Parent, ExpectedMsg) ->
    receive
	go_ahead -> ok
    end,
    {message_queue_len,1} = process_info(self(), message_queue_len),
    erlang:hibernate(?MODULE, messages_in_queue_restart,
		     [Parent,ExpectedMsg]).

messages_in_queue_restart(Parent, ExpectedMessage) ->
    ?line receive
	      ExpectedMessage ->
		  Parent ! done;
	      Other ->
		  io:format("~p\n", [Other]),
		  ?t:fail(unexpected_message)
	  end,
    ok.


%%%
%%% Test that trying to hibernate to an undefined MFA gives the correct
%%% exit behavior.
%%%

undefined_mfa(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Pid = spawn_link(fun() ->
				   %% Will be a call_only instruction.
				   erlang:hibernate(?MODULE, blarf, []) end),
    ?line Pid ! {a,message},
    ?line receive
	      {'EXIT',Pid,{undef,Undef}} ->
		  io:format("~p\n", [Undef]),
		  ok;
	      Other ->
		  ?line io:format("~p\n", [Other]),
		  ?line ?t:fail(unexpected_message)
	  end,
    undefined_mfa_1().

undefined_mfa_1() ->
    ?line Pid = spawn_link(fun() ->
				   %% Force a call_last instruction by calling bar()
				   %% (if that is not obvious).
				   bar(),
				   erlang:hibernate(?MODULE, blarf, [])
			   end),
    ?line Pid ! {another,message},
    ?line receive
	      {'EXIT',Pid,{undef,Undef}} ->
		  io:format("~p\n", [Undef]),
		  ok;
	      Other ->
		  ?line io:format("~p\n", [Other]),
		  ?line ?t:fail(unexpected_message)
	  end,
    ok.

bar() ->
    ok.

%%
%% No heap
%%

no_heap(doc) -> [];
no_heap(suite) -> [];
no_heap(Config) when is_list(Config) ->
    ?line H = spawn_link(fun () -> clean_dict(), no_heap_loop() end),
    ?line lists:foreach(fun (_) ->
				wait_until(fun () -> is_hibernated(H) end),
				?line [{heap_size,1},
				       {total_heap_size,1}]
				    = process_info(H,
						   [heap_size,
						    total_heap_size]),
				receive after 10 -> ok end,
				H ! again
			end,
			lists:seq(1, 100)),
    ?line unlink(H),
    ?line exit(H, bye).

no_heap_loop() ->
    flush(),
    erlang:hibernate(?MODULE, no_heap_loop, []).

clean_dict() ->
    {dictionary, Dict} = process_info(self(), dictionary),
    lists:foreach(fun ({Key, _}) -> erase(Key) end, Dict).

%%
%% Misc
%%

is_hibernated(P) ->
    case process_info(P, [current_function, status]) of
	[{current_function, {erlang, hibernate, _}},
	 {status, waiting}] ->
	    true;
	_ ->
	    false
    end.

flush() ->
    receive
	_Msg -> flush()
    after 0 ->
	    ok
    end.
	   

wait_until(Fun) ->
    case catch Fun() of
	true -> ok;
	_ -> receive after 10 -> wait_until(Fun) end
    end.
