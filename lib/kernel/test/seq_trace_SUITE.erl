%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
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
-module(seq_trace_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).
-export([token_set_get/1, tracer_set_get/1, print/1,
	 send/1, distributed_send/1, recv/1, distributed_recv/1,
	 trace_exit/1, distributed_exit/1, call/1, port/1,
	 match_set_seq_token/1, gc_seq_token/1]).

% internal exports
-export([simple_tracer/2, one_time_receiver/0, one_time_receiver/1,
	 start_tracer/0, stop_tracer/1, 
	 do_match_set_seq_token/1, do_gc_seq_token/1, countdown_start/2]).

						%-define(line_trace, 1).
-include_lib("test_server/include/test_server.hrl").

-compile(no_native).

-define(default_timeout, ?t:minutes(1)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [token_set_get, tracer_set_get, print, send,
     distributed_send, recv, distributed_recv, trace_exit,
     distributed_exit, call, port, match_set_seq_token,
     gc_seq_token].

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


init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%% Verifies that the set_token and get_token functions work as expected

token_set_get(doc) -> [];
token_set_get(suite) -> [];
token_set_get(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line seq_trace:reset_trace(),
    %% Test that initial seq_trace is disabled
    ?line [] = seq_trace:get_token(),
    %% Test setting and reading the different fields
    ?line 0 = seq_trace:set_token(label,17),
    ?line {label,17} = seq_trace:get_token(label),
    ?line false = seq_trace:set_token(print,true),
    ?line {print,true} = seq_trace:get_token(print),
    ?line false = seq_trace:set_token(send,true),
    ?line {send,true} = seq_trace:get_token(send),
    ?line false = seq_trace:set_token('receive',true),
    ?line {'receive',true} = seq_trace:get_token('receive'),
    ?line false = seq_trace:set_token(timestamp,true),
    ?line {timestamp,true} = seq_trace:get_token(timestamp),
    %% Check the whole token
    ?line {15,17,0,Self,0} = seq_trace:get_token(), % all flags are set
    %% Test setting and reading the 'serial' field
    ?line {0,0} = seq_trace:set_token(serial,{3,5}),
    ?line {serial,{3,5}} = seq_trace:get_token(serial),
    %% Check the whole token, test that a whole token can be set and get
    ?line {15,17,5,Self,3} = seq_trace:get_token(),
    ?line seq_trace:set_token({15,19,7,Self,5}),
    ?line {15,19,7,Self,5} = seq_trace:get_token(),
    %% Check that receive timeout does not reset token
    ?line receive after 0 -> ok end,
    ?line {15,19,7,Self,5} = seq_trace:get_token(),
    %% Check that token can be unset
    ?line {15,19,7,Self,5} = seq_trace:set_token([]),
    ?line [] = seq_trace:get_token(),
    %% Check that Previous serial counter survived unset token
    ?line 0 = seq_trace:set_token(label, 17),
    ?line {0,17,0,Self,5} = seq_trace:get_token(),
    %% Check that reset_trace resets the token and clears
    %% the Previous serial counter
    ?line seq_trace:reset_trace(),
    ?line [] = seq_trace:get_token(),
    ?line 0 = seq_trace:set_token(label, 19),
    ?line {0,19,0,Self,0} = seq_trace:get_token(),
    %% Cleanup
    ?line seq_trace:reset_trace(),
    ok.

tracer_set_get(doc) -> [];
tracer_set_get(suite) -> [];
tracer_set_get(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line seq_trace:set_system_tracer(self()),
    ?line Self = seq_trace:get_system_tracer(),
    ?line Self = seq_trace:set_system_tracer(false),
    ?line false = seq_trace:get_system_tracer(),

    %% Set the system tracer to a port.

    ?line Port = load_tracer(Config),
    ?line seq_trace:set_system_tracer(Port),
    ?line Port = seq_trace:get_system_tracer(),
    ?line Port = seq_trace:set_system_tracer(false),
    ?line false = seq_trace:get_system_tracer(),
    ok.

print(doc) -> [];
print(suite) -> [];
print(Config) when is_list(Config) ->
    ?line start_tracer(),
    ?line seq_trace:set_token(print,true),
    ?line seq_trace:print(0,print1),
    ?line seq_trace:print(1,print2),
    ?line seq_trace:print(print3),
    ?line seq_trace:reset_trace(),
    ?line [{0,{print,_,_,[],print1}},
	   {0,{print,_,_,[],print3}}] = stop_tracer(2).
    
send(doc) -> [];
send(suite) -> [];
send(Config) when is_list(Config) ->
    ?line seq_trace:reset_trace(),
    ?line start_tracer(),
    ?line Receiver = spawn(?MODULE,one_time_receiver,[]),
    ?line seq_trace:set_token(send,true),
    ?line Receiver ! send,
    ?line Self = self(),
    ?line seq_trace:reset_trace(),
    ?line [{0,{send,_,Self,Receiver,send}}] = stop_tracer(1).

distributed_send(doc) -> [];
distributed_send(suite) -> [];
distributed_send(Config) when is_list(Config) ->
    ?line {ok,Node} = start_node(seq_trace_other,[]),
    ?line {_,Dir} = code:is_loaded(?MODULE),
    ?line Mdir = filename:dirname(Dir),
    ?line true = rpc:call(Node,code,add_patha,[Mdir]),
    ?line seq_trace:reset_trace(),
    ?line start_tracer(),
    ?line Receiver = spawn(Node,?MODULE,one_time_receiver,[]),
    ?line seq_trace:set_token(send,true),
    ?line Receiver ! send,
    ?line Self = self(),
    ?line seq_trace:reset_trace(),
    ?line stop_node(Node),
    ?line [{0,{send,_,Self,Receiver,send}}] = stop_tracer(1).

recv(doc) -> [];
recv(suite) -> [];
recv(Config) when is_list(Config) ->
    ?line seq_trace:reset_trace(),
    ?line start_tracer(),
    ?line Receiver = spawn(?MODULE,one_time_receiver,[]),
    ?line seq_trace:set_token('receive',true),
    ?line Receiver ! 'receive',
    %% let the other process receive the message:
    ?line receive after 1 -> ok end,
    ?line Self = self(),
    ?line seq_trace:reset_trace(),
    ?line [{0,{'receive',_,Self,Receiver,'receive'}}] = stop_tracer(1).

distributed_recv(doc) -> [];
distributed_recv(suite) -> [];
distributed_recv(Config) when is_list(Config) ->
    ?line {ok,Node} = start_node(seq_trace_other,[]),
    ?line {_,Dir} = code:is_loaded(?MODULE),
    ?line Mdir = filename:dirname(Dir),
    ?line true = rpc:call(Node,code,add_patha,[Mdir]),
    ?line seq_trace:reset_trace(),
    ?line rpc:call(Node,?MODULE,start_tracer,[]),
    ?line Receiver = spawn(Node,?MODULE,one_time_receiver,[]),
    ?line seq_trace:set_token('receive',true),
    ?line Receiver ! 'receive',
    %% let the other process receive the message:
    ?line receive after 1 -> ok end,
    ?line Self = self(),
    ?line seq_trace:reset_trace(),
    ?line Result = rpc:call(Node,?MODULE,stop_tracer,[1]),
    ?line stop_node(Node),
    ?line ok = io:format("~p~n",[Result]),
    ?line [{0,{'receive',_,Self,Receiver,'receive'}}] = Result.

trace_exit(doc) -> [];
trace_exit(suite) -> [];
trace_exit(Config) when is_list(Config) ->
    ?line seq_trace:reset_trace(),
    ?line start_tracer(),
    ?line Receiver = spawn_link(?MODULE, one_time_receiver, [exit]),
    ?line process_flag(trap_exit, true),
    ?line seq_trace:set_token(send,true),
    ?line Receiver ! {before, exit},
    %% let the other process receive the message:
    ?line receive
	      {'EXIT', Receiver, {exit, {before, exit}}} ->
		  seq_trace:set_token([]);
	      Other ->
		  seq_trace:set_token([]),
		  ?t:fail({received, Other})
	  end,
    ?line Self = self(),
    ?line Result = stop_tracer(2),
    ?line seq_trace:reset_trace(),
    ?line ok = io:format("~p~n", [Result]),
    ?line [{0, {send, {0,1}, Self, Receiver, {before, exit}}}, 
	   {0, {send, {1,2}, Receiver, Self,
	       {'EXIT', Receiver, {exit, {before, exit}}}}}] = Result.

distributed_exit(doc) -> [];
distributed_exit(suite) -> [];
distributed_exit(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(seq_trace_other, []),
    ?line {_, Dir} = code:is_loaded(?MODULE),
    ?line Mdir = filename:dirname(Dir),
    ?line true = rpc:call(Node, code, add_patha, [Mdir]),
    ?line seq_trace:reset_trace(),
    ?line rpc:call(Node, ?MODULE, start_tracer,[]),
    ?line Receiver = spawn_link(Node, ?MODULE, one_time_receiver, [exit]),
    ?line process_flag(trap_exit, true),
    ?line seq_trace:set_token(send, true),
    ?line Receiver ! {before, exit},
    %% let the other process receive the message:
    ?line receive
	      {'EXIT', Receiver, {exit, {before, exit}}} ->
		  seq_trace:set_token([]);
	      Other ->
		  seq_trace:set_token([]),
		  ?t:fail({received, Other})
	  end,
    ?line Self = self(),
    ?line Result = rpc:call(Node, ?MODULE, stop_tracer, [1]),
    ?line seq_trace:reset_trace(),
    ?line stop_node(Node),
    ?line ok = io:format("~p~n", [Result]),
    ?line [{0, {send, {1, 2}, Receiver, Self, 
		{'EXIT', Receiver, {exit, {before, exit}}}}}] = Result.

call(doc) -> 
    "Tests special forms {is_seq_trace} and {get_seq_token} "
	"in trace match specs.";
call(suite) -> 
    [];
call(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line seq_trace:reset_trace(),
    ?line TrA = transparent_tracer(),
    ?line 1 = 
	erlang:trace(Self, true, 
		     [call, set_on_spawn, {tracer, TrA(pid)}]),
    ?line 1 = 
	erlang:trace_pattern({?MODULE, call_tracee_1, 1},
			     [{'_',
			       [],
			       [{message, {{{self}, {get_seq_token}}}}]}],
			     [local]),
    ?line 1 =
	erlang:trace_pattern({?MODULE, call_tracee_2, 1},
			     [{'_',
			      [{is_seq_trace}],
			      [{message, {{{self}, {get_seq_token}}}}]}],
			     [local]),
    ?line RefA = make_ref(),
    ?line Pid2A = spawn_link(
		    fun() ->
			    receive {_, msg, RefA} -> ok end,
			    RefA = call_tracee_2(RefA),
			    Self ! {self(), msg, RefA}
		   end),
    ?line Pid1A = spawn_link(
		    fun() ->
			    receive {_, msg, RefA} -> ok end,
			    RefA = call_tracee_1(RefA),
			    Pid2A ! {self(), msg, RefA}
		    end),
    ?line Pid1A ! {Self, msg, RefA},
    %% The message is passed Self -> Pid1B -> Pid2B -> Self.
    %% Traced functions are called in Pid1B and Pid2B. 
    ?line receive {Pid2A, msg, RefA} -> ok end,
    %% Only call_tracee1 will be traced since the guard for 
    %% call_tracee2 requires a sequential trace. The trace
    %% token is undefined.
    ?line Token2A = [],
    ?line {ok, [{trace, Pid1A, call, 
		 {?MODULE, call_tracee_1, [RefA]},
		 {Pid1A, Token2A}}]} = 
	TrA({stop, 1}),

    ?line seq_trace:reset_trace(),

    ?line TrB = transparent_tracer(),
    ?line 1 = 
	erlang:trace(Self, true, 
		     [call, set_on_spawn, {tracer, TrB(pid)}]),
    ?line Label = 17,
    ?line seq_trace:set_token(label, Label), % Token enters here!!
    ?line RefB = make_ref(),
    ?line Pid2B = spawn_link(
		    fun() ->
			    receive {_, msg, RefB} -> ok end,
			    RefB = call_tracee_2(RefB),
			    Self ! {self(), msg, RefB}
		   end),
    ?line Pid1B = spawn_link(
		    fun() ->
			    receive {_, msg, RefB} -> ok end,
			    RefB = call_tracee_1(RefB),
			    Pid2B ! {self(), msg, RefB}
		    end),
    ?line Pid1B ! {Self, msg, RefB},
    %% The message is passed Self -> Pid1B -> Pid2B -> Self, and the 
    %% seq_trace token follows invisibly. Traced functions are 
    %% called in Pid1B and Pid2B. Seq_trace flags == 0 so no
    %% seq_trace messages are generated.
    ?line receive {Pid2B, msg, RefB} -> ok end,
    %% The values of these counters {.., 1, _, 0}, {.., 2, _, 1}
    %% depend on that seq_trace has been reset just before this test.
    ?line Token1B = {0, Label, 1, Self, 0},
    ?line Token2B = {0, Label, 2, Pid1B, 1},
    ?line {ok, [{trace, Pid1B, call, 
		 {?MODULE, call_tracee_1, [RefB]},
		 {Pid1B, Token1B}},
		{trace, Pid2B, call, 
		 {?MODULE, call_tracee_2, [RefB]},
		 {Pid2B, Token2B}}]} =
	TrB({stop,2}),
    ?line seq_trace:reset_trace(),
    ok.

port(doc) ->
    "Send trace messages to a port.";
port(suite) -> [];
port(Config) when is_list(Config) ->
    ?line Port = load_tracer(Config),
    ?line seq_trace:set_system_tracer(Port),

    ?line seq_trace:set_token(print, true),
    ?line Small = [small,term],
    ?line seq_trace:print(0, Small),
    ?line case get_port_message(Port) of
	      {seq_trace,0,{print,_,_,[],Small}} ->
		  ok;
	      Other ->
		  ?line seq_trace:reset_trace(),
		  ?line ?t:fail({unexpected,Other})
	  end,
    %% OTP-4218 Messages from ports should not affect seq trace token.
    %%
    %% Check if trace token still is active on this process after
    %% the get_port_message/1 above that receives from a port.
    ?line OtherSmall = [other | Small],
    ?line seq_trace:print(0, OtherSmall),
    ?line seq_trace:reset_trace(),
    ?line case get_port_message(Port) of
	      {seq_trace,0,{print,_,_,[],OtherSmall}} ->
		  ok;
	      Other1 ->
		  ?line ?t:fail({unexpected,Other1})
	  end,


    ?line seq_trace:set_token(print, true),
    ?line Huge = huge_data(),
    ?line seq_trace:print(0, Huge),
    ?line seq_trace:reset_trace(),
    ?line case get_port_message(Port) of
	      {seq_trace,0,{print,_,_,[],Huge}} ->
		  ok;
	      Other2 ->
		  ?line ?t:fail({unexpected,Other2})
	  end,
    ok.

get_port_message(Port) ->
    receive
	{Port,{data,Bin}} when is_binary(Bin) ->
	    binary_to_term(Bin);
	Other ->
	    ?t:fail({unexpected,Other})
    after 5000 ->
	    ?t:fail(timeout)
    end.



match_set_seq_token(suite) ->
    [];
match_set_seq_token(doc) ->
    ["Tests that match spec function set_seq_token does not "
     "corrupt the heap"];
match_set_seq_token(Config) when is_list(Config) ->
    ?line Parent = self(),
    ?line Timetrap = test_server:timetrap(test_server:seconds(20)),
    %% OTP-4222 Match spec 'set_seq_token' corrupts heap
    %%
    %% This test crashes the emulator if the bug in question is present,
    %% it is therefore done in a slave node.
    %%
    %% All the timeout stuff is here to get decent accuracy of the error
    %% return value, instead of just 'timeout'.
    %
    ?line {ok, Sandbox} = start_node(seq_trace_other, []),
    ?line true = rpc:call(Sandbox, code, add_patha, 
			  [filename:dirname(code:which(?MODULE))]),
    ?line Lbl = 4711,
    %% Do the possibly crashing test
    ?line P1 =
	spawn(
	  fun () ->
		  Parent ! {self(),
			    rpc:call(Sandbox, 
				     ?MODULE, do_match_set_seq_token, [Lbl])}
	  end),
    %% Probe the node with a simple rpc request, to see if it is alive.
    ?line P2 =
	spawn(
	  fun () ->
		  receive after 4000 -> ok end,
		  Parent ! {self(), rpc:call(Sandbox, erlang, abs, [-1])}
	  end),
    %% If the test node hangs completely, this timer expires.
    ?line R3 = erlang:start_timer(8000, self(), void),
    %%
    ?line {ok, Log} = 
	receive
	    {P1, Result} ->
		exit(P2, done),
		erlang:cancel_timer(R3),
		Result;
	    {P2, 1} ->
		exit(P1, timeout),
		erlang:cancel_timer(R3),
		{error, "Test process hung"};
	    {timeout, R3, _} ->
		exit(P1, timeout),
		exit(P2, timeout),
		{error, "Test node hung"}
	end,
    ?line ok = check_match_set_seq_token_log(Lbl, Log),
    %%
    ?line stop_node(Sandbox),
    ?line test_server:timetrap_cancel(Timetrap),
    ok.

%% OTP-4222 Match spec 'set_seq_token' corrupts heap
%%
%% The crashing test goes as follows:
%%
%% One trigger function calls match spec function {set_seq_token, _, _},
%% which when faulty corrupts the heap. It is assured that the process
%% in question has a big heap and recently garbage collected so there 
%% will be room on the heap, which is necessary for the crash to happen.
%%
%% Then two processes bounces a few messages between each other, and if
%% the heap is crashed the emulator crashes, or the triggering process's
%% loop data gets corrupted so the loop never ends.
do_match_set_seq_token(Label) ->
    seq_trace:reset_trace(),
    Tr = transparent_tracer(),
    TrPid = Tr(pid),
    erlang:trace_pattern({?MODULE, '_', '_'}, 
			 [{'_', 
			   [{is_seq_trace}], 
			   [{message, {get_seq_token}}]}], 
			 [local]),
    erlang:trace_pattern({?MODULE, countdown, 2}, 
			 [{'_', 
			   [], 
			   [{set_seq_token, label, Label},
			    {message, {get_seq_token}}]}],
			 [local]),
    erlang:trace(new, true, [call, {tracer, TrPid}]),
    Ref = make_ref(),
    Bounce = spawn(fun () -> bounce(Ref) end),
    Mref = erlang:monitor(process, Bounce),
    _Countdown = erlang:spawn_opt(?MODULE, countdown_start, [Bounce, Ref],
				 [{min_heap_size, 4192}]),
    receive 
	{'DOWN', Mref, _, _, normal} -> 
	    Result = Tr({stop, 0}),
	    seq_trace:reset_trace(),
	    erlang:trace(new, false, [call]),
	    Result;
	{'DOWN', Mref, _, _, Reason} ->
	    Tr({stop, 0}),
	    seq_trace:reset_trace(),
	    erlang:trace(new, false, [call]),
	    {error, Reason}
    end.

check_match_set_seq_token_log(
  Label,
  [{trace,C,call,{?MODULE,countdown,[B,Ref]},  {0,Label,0,C,0}},
   {trace,C,call,{?MODULE,countdown,[B,Ref,3]},{0,Label,0,C,0}},
   {trace,B,call,{?MODULE,bounce,   [Ref]},    {0,Label,2,B,1}},
   {trace,C,call,{?MODULE,countdown,[B,Ref,2]},{0,Label,2,B,1}},
   {trace,B,call,{?MODULE,bounce,   [Ref]},    {0,Label,4,B,3}},
   {trace,C,call,{?MODULE,countdown,[B,Ref,1]},{0,Label,4,B,3}},
   {trace,B,call,{?MODULE,bounce,   [Ref]},    {0,Label,6,B,5}},
   {trace,C,call,{?MODULE,countdown,[B,Ref,0]},{0,Label,6,B,5}}
  ]) ->
    ok;
check_match_set_seq_token_log(_Label, Log) ->
    {error, Log}.

countdown_start(Bounce, Ref) ->
    %% This gc and the increased heap size of this process ensures that
    %% the match spec executed for countdown/2 has got heap space for
    %% the trace token, so the heap gets trashed according to OTP-4222.
    erlang:garbage_collect(),
    countdown(Bounce, Ref).

countdown(Bounce, Ref) ->
    countdown(Bounce, Ref, 3).

countdown(Bounce, Ref, 0) ->
    Bounce ! Ref;
countdown(Bounce, Ref, Cnt) ->
    Tag = make_ref(),
    Bounce ! {Ref, self(), {Tag, Cnt}},
    receive {Tag, Cnt} -> countdown(Bounce, Ref, Cnt-1) end.

bounce(Ref) ->
    receive 
	Ref ->
	    ok;
	{Ref, Dest, Msg} ->
	    Dest ! Msg,
	    bounce(Ref)
    end.



gc_seq_token(suite) ->
    [];
gc_seq_token(doc) ->
    ["Tests that a seq_trace token on a message in the inqueue ",
     "can be garbage collected."];
gc_seq_token(Config) when is_list(Config) ->
    ?line Parent = self(),
    ?line Timetrap = test_server:timetrap(test_server:seconds(20)),
    %% OTP-4555 Seq trace token causes free mem read in gc
    %%
    %% This test crashes the emulator if the bug in question is present,
    %% it is therefore done in a slave node.
    %%
    %% All the timeout stuff is here to get decent accuracy of the error
    %% return value, instead of just 'timeout'.
    %
    ?line {ok, Sandbox} = start_node(seq_trace_other, []),
    ?line true = rpc:call(Sandbox, code, add_patha, 
			  [filename:dirname(code:which(?MODULE))]),
    ?line Label = 4711,
    %% Do the possibly crashing test
    ?line P1 =
	spawn(
	  fun () ->
 		  Parent ! {self(),
			    rpc:call(Sandbox, 
				     ?MODULE, do_gc_seq_token, [Label])}
	  end),
    %% Probe the node with a simple rpc request, to see if it is alive.
    ?line P2 =
	spawn(
	  fun () ->
		  receive after 4000 -> ok end,
		  Parent ! {self(), rpc:call(Sandbox, erlang, abs, [-1])}
	  end),
    %% If the test node hangs completely, this timer expires.
    ?line R3 = erlang:start_timer(8000, self(), void),
    %%
    ?line ok = 
	receive
	    {P1, Result} ->
		exit(P2, done),
		erlang:cancel_timer(R3),
		Result;
	    {P2, 1} ->
		exit(P1, timeout),
		erlang:cancel_timer(R3),
		{error, "Test process hung"};
	    {timeout, R3, _} ->
		exit(P1, timeout),
		exit(P2, timeout),
		{error, "Test node hung"}
	end,
    %%
    ?line stop_node(Sandbox),
    ?line test_server:timetrap_cancel(Timetrap),
    ok.

do_gc_seq_token(Label) ->
    Parent = self(),
    Comment =       
	{"OTP-4555 Seq trace token causes free mem read in gc\n"
	 "\n"
	 "The crashing test goes as follows:\n"
	 "\n"
	 "Put a message with seq_trace token in the inqueue,\n"
	 "Grow the process heap big enough to become mmap'ed\n"
	 "and force a garbage collection using large terms\n"
	 "to get a test_heap instruction with a big size value.\n"
	 "Then try to trick the heap into shrinking.\n"
	 "\n"
	 "All this to make the GC move the heap between memory blocks.\n"},
    seq_trace:reset_trace(),
    Child = spawn_link(
	      fun() ->
		      receive {Parent, no_seq_trace_token} -> ok end,
		      do_grow(Comment, 256*1024, []),
		      do_shrink(10),
		      receive {Parent, seq_trace_token} -> ok end,
		      Parent ! {self(), {token, seq_trace:get_token(label)}}
	      end),
    seq_trace:set_token(label, Label),
    Child ! {Parent, seq_trace_token},
    seq_trace:set_token([]),
    Child ! {Parent, no_seq_trace_token},
    receive 
	{Child, {token, {label, Label}}} -> 
	    ok;
	{Child, {token, Other}} ->
	    {error, Other}
    end.

do_grow(_, 0, Acc) ->
    Acc;
do_grow(E, N, Acc) ->
    do_grow(E, N-1, [E | Acc]).

do_shrink(0) ->
    ok;
do_shrink(N) ->
    erlang:garbage_collect(),
    do_shrink(N-1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal help functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Call trace targets

call_tracee_1(X) ->
    X.

call_tracee_2(X) ->
    X.


transparent_tracer() ->
    Ref = make_ref(),
    Loop = 
	fun(Fun, Log, LN) ->
		receive
		    {stop, MinLN, Ref, From} when LN >= MinLN ->
			From ! {log, Ref, lists:reverse(Log)};
		    Entry when is_tuple(Entry) == false; element(1, Entry) /= stop ->
			Fun(Fun, [Entry | Log], LN+1)
		end
	end,
    Self = self(),
    Pid = 
	spawn(fun() ->
		      seq_trace:set_system_tracer(self()),
		      Self ! {started, Ref},
		      Loop(Loop, [], 0) 
	      end),
    receive {started, Ref} -> ok end,
    fun(pid) ->
	    Pid;
       ({stop, N}) when is_integer(N), N >= 0 ->
	    Mref = erlang:monitor(process, Pid),
	    receive
		{'DOWN', Mref, _, _, _} ->
		    {error, not_started}
	    after 0 ->
		    DeliverRef = erlang:trace_delivered(all),
		    receive
			{trace_delivered,_,DeliverRef} -> ok
		    end,
		    Pid ! {stop, N, Ref, self()},
		    receive {'DOWN', Mref, _, _, _} -> ok end,
		    receive {log, Ref, Log} -> 
			    {ok, Log} 
		    end
	    end
    end.



one_time_receiver() ->
    receive _Term -> ok
    end.
	
one_time_receiver(exit) ->
    receive Term ->
	    exit({exit, Term}) 
    end.

simple_tracer(Data, DN) ->
    receive
	{seq_trace,Label,Info,Ts} ->
	    simple_tracer([{Label,Info,Ts}|Data], DN+1);
	{seq_trace,Label,Info} ->
	    simple_tracer([{Label,Info}|Data], DN+1);
	{stop,N,From} when DN >= N ->
	    From ! {tracerlog,lists:reverse(Data)}
    end.

stop_tracer(N) when is_integer(N) ->
    case catch (seq_trace_SUITE_tracer ! {stop,N,self()}) of
	{'EXIT', _} ->
	    {error, not_started};
	_ ->
	    receive
		{tracerlog,Data} ->
		    Data
	    after 1000 ->
		    {error,timeout}
	    end
    end.

start_tracer() ->
    stop_tracer(0),
    Pid = spawn(?MODULE,simple_tracer,[[], 0]),
    register(seq_trace_SUITE_tracer,Pid),
    seq_trace:set_system_tracer(Pid),
    Pid.

    
	
start_node(Name, Param) ->
    test_server:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    test_server:stop_node(Node).

load_tracer(Config) ->
    Path = ?config(data_dir, Config),
    ok = erl_ddll:load_driver(Path, echo_drv),
    open_port({spawn,echo_drv}, [eof,binary]).

huge_data() -> huge_data(16384).
huge_data(0) -> [];
huge_data(N) when N rem 2 == 0 ->
    P = huge_data(N div 2),
    [P|P];
huge_data(N) ->
    P = huge_data(N div 2),
    [16#1234566,P|P].
