%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
-module(seq_trace_SUITE).

%% label_capability_mismatch needs to run a part of the test on an OTP 20 node.
-compile(r20).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).
-export([token_set_get/1, tracer_set_get/1, print/1,
         old_heap_token/1,
	 send/1, distributed_send/1, recv/1, distributed_recv/1,
	 trace_exit/1, distributed_exit/1, call/1, port/1,
         port_clean_token/1,
	 match_set_seq_token/1, gc_seq_token/1, label_capability_mismatch/1,
         send_literal/1]).

%% internal exports
-export([simple_tracer/2, one_time_receiver/0, one_time_receiver/1,
         n_time_receiver/1,
	 start_tracer/0, stop_tracer/1, 
	 do_match_set_seq_token/1, do_gc_seq_token/1, countdown_start/2]).

-include_lib("common_test/include/ct.hrl").

-define(TIMESTAMP_MODES, [no_timestamp,
			  timestamp,
			  monotonic_timestamp,
			  strict_monotonic_timestamp]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [token_set_get, tracer_set_get, print, send, send_literal,
     distributed_send, recv, distributed_recv, trace_exit,
     old_heap_token,
     distributed_exit, call, port, match_set_seq_token,
     port_clean_token,
     gc_seq_token, label_capability_mismatch].

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
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Verifies that the set_token and get_token functions work as expected

token_set_get(Config) when is_list(Config) ->
    do_token_set_get(timestamp),
    do_token_set_get(monotonic_timestamp),
    do_token_set_get(strict_monotonic_timestamp).
    
do_token_set_get(TsType) ->
    io:format("Testing ~p~n", [TsType]),
    Flags = case TsType of
		timestamp -> 15;
		strict_monotonic_timestamp -> 23;
		monotonic_timestamp -> 39
	    end,
    Self = self(),
    seq_trace:reset_trace(),
    %% Test that initial seq_trace is disabled
    [] = seq_trace:get_token(),
    %% Test setting and reading the different fields
    0 = seq_trace:set_token(label,{my_label,1}),
    {label,{my_label,1}} = seq_trace:get_token(label),
    false = seq_trace:set_token(print,true),
    {print,true} = seq_trace:get_token(print),
    false = seq_trace:set_token(send,true),
    {send,true} = seq_trace:get_token(send),
    false = seq_trace:set_token('receive',true),
    {'receive',true} = seq_trace:get_token('receive'),
    false = seq_trace:set_token(TsType,true),
    {TsType,true} = seq_trace:get_token(TsType),
    %% Check the whole token
    {Flags,{my_label,1},0,Self,0} = seq_trace:get_token(), % all flags are set
    %% Test setting and reading the 'serial' field
    {0,0} = seq_trace:set_token(serial,{3,5}),
    {serial,{3,5}} = seq_trace:get_token(serial),
    %% Check the whole token, test that a whole token can be set and get
    {Flags,{my_label,1},5,Self,3} = seq_trace:get_token(),
    seq_trace:set_token({Flags,19,7,Self,5}),
    {Flags,19,7,Self,5} = seq_trace:get_token(),
    %% Check that receive timeout does not reset token
    receive after 0 -> ok end,
    {Flags,19,7,Self,5} = seq_trace:get_token(),
    %% Check that token can be unset
    {Flags,19,7,Self,5} = seq_trace:set_token([]),
    [] = seq_trace:get_token(),
    %% Check that Previous serial counter survived unset token
    0 = seq_trace:set_token(label, 17),
    {0,17,0,Self,5} = seq_trace:get_token(),
    %% Check that reset_trace resets the token and clears
    %% the Previous serial counter
    seq_trace:reset_trace(),
    [] = seq_trace:get_token(),
    0 = seq_trace:set_token(label, 19),
    {0,19,0,Self,0} = seq_trace:get_token(),
    %% Cleanup
    seq_trace:reset_trace(),
    ok.

tracer_set_get(Config) when is_list(Config) ->
    Self = self(),
    seq_trace:set_system_tracer(self()),
    Self = seq_trace:get_system_tracer(),
    Self = seq_trace:set_system_tracer(false),
    false = seq_trace:get_system_tracer(),

    %% Set the system tracer to a port.

    Port = load_tracer(Config),
    seq_trace:set_system_tracer(Port),
    Port = seq_trace:get_system_tracer(),
    Port = seq_trace:set_system_tracer(false),
    false = seq_trace:get_system_tracer(),
    ok.

print(Config) when is_list(Config) ->
    [do_print(TsType, Label) || TsType <- ?TIMESTAMP_MODES,
                                Label <- [17, "label"]].
    
do_print(TsType, Label) ->
    start_tracer(),
    seq_trace:set_token(label, Label),
    set_token_flags([print, TsType]),
    seq_trace:print(Label,print1),
    seq_trace:print(1,print2),
    seq_trace:print(print3),
    seq_trace:reset_trace(),
    [{Label,{print,_,_,[],print1}, Ts0},
     {Label,{print,_,_,[],print3}, Ts1}] = stop_tracer(2),
    check_ts(TsType, Ts0),
    check_ts(TsType, Ts1).

send(Config) when is_list(Config) ->
    lists:foreach(fun do_send/1, ?TIMESTAMP_MODES).

do_send(TsType) ->
    do_send(TsType, send).

do_send(TsType, Msg) ->
    seq_trace:reset_trace(),
    start_tracer(),
    Receiver = spawn(?MODULE,n_time_receiver,[2]),
    register(n_time_receiver, Receiver),
    Label = make_ref(),
    seq_trace:set_token(label,Label),
    set_token_flags([send, TsType]),
    Receiver ! Msg,
    n_time_receiver ! Msg,
    Self = self(),
    seq_trace:reset_trace(),
    [{Label,{send,_,Self,Receiver,Msg}, Ts1},
     %% Apparently named local destination process is traced as pid (!?)
     {Label,{send,_,Self,Receiver,Msg}, Ts2}
    ] = stop_tracer(2),
    check_ts(TsType, Ts1),
    check_ts(TsType, Ts2).

%% This testcase tests that we do not segfault when we have a
%% literal as the message and the message is copied onto the
%% heap during a GC.
send_literal(Config) when is_list(Config) ->
    lists:foreach(fun do_send_literal/1,
                  [atom, make_ref(), ets:new(hej,[]), 1 bsl 64,
                   "gurka", {tuple,test,with,#{}}, #{}]).

do_send_literal(Msg) ->
    N = 10000,
    seq_trace:reset_trace(),
    start_tracer(),
    Label = make_ref(),
    seq_trace:set_token(label,Label),
    set_token_flags([send, 'receive', no_timestamp]),
    Receiver = spawn_link(fun() -> receive ok -> ok end end),
    [Receiver ! Msg || _ <- lists:seq(1, N)],
    erlang:garbage_collect(Receiver),
    [Receiver ! Msg || _ <- lists:seq(1, N)],
    erlang:garbage_collect(Receiver),
    Self = self(),
    seq_trace:reset_trace(),
    [{Label,{send,_,Self,Receiver,Msg}, Ts} | _] = stop_tracer(N),
    check_ts(no_timestamp, Ts).

distributed_send(Config) when is_list(Config) ->
    lists:foreach(fun do_distributed_send/1, ?TIMESTAMP_MODES).

do_distributed_send(TsType) ->
    {ok,Node} = start_node(seq_trace_other,[]),
    {_,Dir} = code:is_loaded(?MODULE),
    Mdir = filename:dirname(Dir),
    true = rpc:call(Node,code,add_patha,[Mdir]),
    seq_trace:reset_trace(),
    start_tracer(),
    Receiver = spawn(Node,?MODULE,n_time_receiver,[2]),
    true = rpc:call(Node,erlang,register,[n_time_receiver, Receiver]),

    %% Make sure complex labels survive the trip.
    Label = make_ref(),
    seq_trace:set_token(label,Label),
    set_token_flags([send,TsType]),

    Receiver ! send,
    {n_time_receiver, Node} ! "dsend",

    Self = self(),
    seq_trace:reset_trace(),
    stop_node(Node),
    [{Label,{send,_,Self,Receiver,send}, Ts1},
     {Label,{send,_,Self,{n_time_receiver,Node}, "dsend"}, Ts2}
    ] = stop_tracer(2),

    check_ts(TsType, Ts1),
    check_ts(TsType, Ts2).


recv(Config) when is_list(Config) ->
    lists:foreach(fun do_recv/1, ?TIMESTAMP_MODES).

do_recv(TsType) ->
    seq_trace:reset_trace(),
    start_tracer(),
    Receiver = spawn(?MODULE,one_time_receiver,[]),
    set_token_flags(['receive',TsType]),
    Receiver ! 'receive',
    %% let the other process receive the message:
    receive after 1 -> ok end,
    Self = self(),
    seq_trace:reset_trace(),
    [{0,{'receive',_,Self,Receiver,'receive'}, Ts}] = stop_tracer(1),
    check_ts(TsType, Ts).

distributed_recv(Config) when is_list(Config) ->
    lists:foreach(fun do_distributed_recv/1, ?TIMESTAMP_MODES).

do_distributed_recv(TsType) ->
    {ok,Node} = start_node(seq_trace_other,[]),
    {_,Dir} = code:is_loaded(?MODULE),
    Mdir = filename:dirname(Dir),
    true = rpc:call(Node,code,add_patha,[Mdir]),
    seq_trace:reset_trace(),
    rpc:call(Node,?MODULE,start_tracer,[]),
    Receiver = spawn(Node,?MODULE,one_time_receiver,[]),

    %% Make sure complex labels survive the trip.
    Label = make_ref(),
    seq_trace:set_token(label,Label),
    set_token_flags(['receive',TsType]),

    Receiver ! 'receive',
    %% let the other process receive the message:
    receive after 1 -> ok end,
    Self = self(),
    seq_trace:reset_trace(),
    Result = rpc:call(Node,?MODULE,stop_tracer,[1]),
    stop_node(Node),
    ok = io:format("~p~n",[Result]),
    [{Label,{'receive',_,Self,Receiver,'receive'}, Ts}] = Result,
    check_ts(TsType, Ts).

trace_exit(Config) when is_list(Config) ->
    lists:foreach(fun do_trace_exit/1, ?TIMESTAMP_MODES).

do_trace_exit(TsType) ->
    seq_trace:reset_trace(),
    start_tracer(),
    Receiver = spawn_link(?MODULE, one_time_receiver, [exit]),
    process_flag(trap_exit, true),

    %% Make sure complex labels survive the trip.
    Label = make_ref(),
    seq_trace:set_token(label,Label),
    set_token_flags([send, TsType]),

    Receiver ! {before, exit},
    %% let the other process receive the message:
    receive
	      {'EXIT', Receiver, {exit, {before, exit}}} ->
		  seq_trace:set_token([]);
	      Other ->
		  seq_trace:set_token([]),
		  ct:fail({received, Other})
	  end,
    Self = self(),
    Result = stop_tracer(2),
    seq_trace:reset_trace(),
    ok = io:format("~p~n", [Result]),
    [{Label, {send, {0,1}, Self, Receiver, {before, exit}}, Ts0},
	   {Label, {send, {1,2}, Receiver, Self,
	       {'EXIT', Receiver, {exit, {before, exit}}}}, Ts1}] = Result,
    check_ts(TsType, Ts0),
    check_ts(TsType, Ts1).

distributed_exit(Config) when is_list(Config) ->
    lists:foreach(fun do_distributed_exit/1, ?TIMESTAMP_MODES).

do_distributed_exit(TsType) ->
    {ok, Node} = start_node(seq_trace_other, []),
    {_, Dir} = code:is_loaded(?MODULE),
    Mdir = filename:dirname(Dir),
    true = rpc:call(Node, code, add_patha, [Mdir]),
    seq_trace:reset_trace(),
    rpc:call(Node, ?MODULE, start_tracer,[]),
    Receiver = spawn_link(Node, ?MODULE, one_time_receiver, [exit]),
    process_flag(trap_exit, true),
    set_token_flags([send, TsType]),
    Receiver ! {before, exit},
    %% let the other process receive the message:
    receive
	      {'EXIT', Receiver, {exit, {before, exit}}} ->
		  seq_trace:set_token([]);
	      Other ->
		  seq_trace:set_token([]),
		  ct:fail({received, Other})
	  end,
    Self = self(),
    Result = rpc:call(Node, ?MODULE, stop_tracer, [1]),
    seq_trace:reset_trace(),
    stop_node(Node),
    ok = io:format("~p~n", [Result]),
    [{0, {send, {1, 2}, Receiver, Self,
		{'EXIT', Receiver, {exit, {before, exit}}}}, Ts}] = Result,
    check_ts(TsType, Ts).

label_capability_mismatch(Config) when is_list(Config) ->
    Releases = ["20_latest"],
    Available = [Rel || Rel <- Releases, test_server:is_release_available(Rel)],
    case Available of
        [] -> {skipped, "No incompatible releases available"};
        _ ->
            lists:foreach(fun do_incompatible_labels/1, Available),
            lists:foreach(fun do_compatible_labels/1, Available),
            ok
    end.

do_incompatible_labels(Rel) ->
    Cookie = atom_to_list(erlang:get_cookie()),
    {ok, Node} = test_server:start_node(
        list_to_atom(atom_to_list(?MODULE)++"_"++Rel), peer,
        [{args, " -setcookie "++Cookie}, {erl, [{release, Rel}]}]),

    {_,Dir} = code:is_loaded(?MODULE),
    Mdir = filename:dirname(Dir),
    true = rpc:call(Node,code,add_patha,[Mdir]),
    seq_trace:reset_trace(),
    true = is_pid(rpc:call(Node,?MODULE,start_tracer,[])),
    Receiver = spawn(Node,?MODULE,one_time_receiver,[]),

    %% This node does not support arbitrary labels, so it must fail with a
    %% timeout as the token is dropped silently.
    seq_trace:set_token(label,make_ref()),
    seq_trace:set_token('receive',true),

    Receiver ! 'receive',
    %% let the other process receive the message:
    receive after 10 -> ok end,
    seq_trace:reset_trace(),

    {error,timeout} = rpc:call(Node,?MODULE,stop_tracer,[1]),
    stop_node(Node),
    ok.

do_compatible_labels(Rel) ->
    Cookie = atom_to_list(erlang:get_cookie()),
    {ok, Node} = test_server:start_node(
        list_to_atom(atom_to_list(?MODULE)++"_"++Rel), peer,
        [{args, " -setcookie "++Cookie}, {erl, [{release, Rel}]}]),

    {_,Dir} = code:is_loaded(?MODULE),
    Mdir = filename:dirname(Dir),
    true = rpc:call(Node,code,add_patha,[Mdir]),
    seq_trace:reset_trace(),
    true = is_pid(rpc:call(Node,?MODULE,start_tracer,[])),
    Receiver = spawn(Node,?MODULE,one_time_receiver,[]),

    %% This node does not support arbitrary labels, but small integers should
    %% still work.
    Label = 1234,
    seq_trace:set_token(label,Label),
    seq_trace:set_token('receive',true),

    Receiver ! 'receive',
    %% let the other process receive the message:
    receive after 10 -> ok end,
    Self = self(),
    seq_trace:reset_trace(),
    Result = rpc:call(Node,?MODULE,stop_tracer,[1]),
    stop_node(Node),
    ok = io:format("~p~n",[Result]),
    [{Label,{'receive',_,Self,Receiver,'receive'}, _}] = Result,
    ok.

call(doc) -> 
    "Tests special forms {is_seq_trace} and {get_seq_token} "
	"in trace match specs.";
call(Config) when is_list(Config) ->
    Self = self(),
    seq_trace:reset_trace(),
    TrA = transparent_tracer(),
    1 =
	erlang:trace(Self, true, 
		     [call, set_on_spawn, {tracer, TrA(pid)}]),
    1 =
	erlang:trace_pattern({?MODULE, call_tracee_1, 1},
			     [{'_',
			       [],
			       [{message, {{{self}, {get_seq_token}}}}]}],
			     [local]),
    1 =
	erlang:trace_pattern({?MODULE, call_tracee_2, 1},
			     [{'_',
			      [{is_seq_trace}],
			      [{message, {{{self}, {get_seq_token}}}}]}],
			     [local]),
    RefA = make_ref(),
    Pid2A = spawn_link(
		    fun() ->
			    receive {_, msg, RefA} -> ok end,
			    RefA = call_tracee_2(RefA),
			    Self ! {self(), msg, RefA}
		   end),
    Pid1A = spawn_link(
		    fun() ->
			    receive {_, msg, RefA} -> ok end,
			    RefA = call_tracee_1(RefA),
			    Pid2A ! {self(), msg, RefA}
		    end),
    Pid1A ! {Self, msg, RefA},
    %% The message is passed Self -> Pid1B -> Pid2B -> Self.
    %% Traced functions are called in Pid1B and Pid2B. 
    receive {Pid2A, msg, RefA} -> ok end,
    %% Only call_tracee1 will be traced since the guard for 
    %% call_tracee2 requires a sequential trace. The trace
    %% token is undefined.
    Token2A = [],
    {ok, [{trace, Pid1A, call,
		 {?MODULE, call_tracee_1, [RefA]},
		 {Pid1A, Token2A}}]} = 
	TrA({stop, 1}),

    seq_trace:reset_trace(),

    TrB = transparent_tracer(),
    1 =
	erlang:trace(Self, true, 
		     [call, set_on_spawn, {tracer, TrB(pid)}]),
    Label = 17,
    seq_trace:set_token(label, Label), % Token enters here!!
    RefB = make_ref(),
    Pid2B = spawn_link(
		    fun() ->
			    receive {_, msg, RefB} -> ok end,
			    RefB = call_tracee_2(RefB),
			    Self ! {self(), msg, RefB}
		   end),
    Pid1B = spawn_link(
		    fun() ->
			    receive {_, msg, RefB} -> ok end,
			    RefB = call_tracee_1(RefB),
			    Pid2B ! {self(), msg, RefB}
		    end),
    Pid1B ! {Self, msg, RefB},
    %% The message is passed Self -> Pid1B -> Pid2B -> Self, and the 
    %% seq_trace token follows invisibly. Traced functions are 
    %% called in Pid1B and Pid2B. Seq_trace flags == 0 so no
    %% seq_trace messages are generated.
    receive {Pid2B, msg, RefB} -> ok end,
    %% The values of these counters {.., 1, _, 0}, {.., 2, _, 1}
    %% depend on that seq_trace has been reset just before this test.
    Token1B = {0, Label, 1, Self, 0},
    Token2B = {0, Label, 2, Pid1B, 1},
    {ok, [{trace, Pid1B, call,
		 {?MODULE, call_tracee_1, [RefB]},
		 {Pid1B, Token1B}},
		{trace, Pid2B, call, 
		 {?MODULE, call_tracee_2, [RefB]},
		 {Pid2B, Token2B}}]} =
	TrB({stop,2}),
    seq_trace:reset_trace(),
    ok.

%% Send trace messages to a port.
port(Config) when is_list(Config) ->
    lists:foreach(fun (TsType) -> do_port(TsType, Config) end,
		  ?TIMESTAMP_MODES).

do_port(TsType, Config) ->
    io:format("Testing ~p~n",[TsType]),
    Port = load_tracer(Config),
    seq_trace:set_system_tracer(Port),

    set_token_flags([print, TsType]),
    Small = [small,term],
    seq_trace:print(0, Small),
    case get_port_message(Port) of
	      {seq_trace,0,{print,_,_,[],Small}} when TsType == no_timestamp ->
		  ok;
	      {seq_trace,0,{print,_,_,[],Small},Ts0} when TsType /= no_timestamp ->
		  check_ts(TsType, Ts0),
		  ok;
	      Other ->
		  seq_trace:reset_trace(),
		  ct:fail({unexpected,Other})
	  end,
    %% OTP-4218 Messages from ports should not affect seq trace token.
    %%
    %% Check if trace token still is active on this process after
    %% the get_port_message/1 above that receives from a port.
    OtherSmall = [other | Small],
    seq_trace:print(0, OtherSmall),
    seq_trace:reset_trace(),
    case get_port_message(Port) of
	      {seq_trace,0,{print,_,_,[],OtherSmall}} when TsType == no_timestamp ->
		  ok;
	      {seq_trace,0,{print,_,_,[],OtherSmall}, Ts1} when TsType /= no_timestamp ->
		  check_ts(TsType, Ts1),
		  ok;
	      Other1 ->
		  ct:fail({unexpected,Other1})
	  end,


    seq_trace:set_token(print, true),
    Huge = huge_data(),
    seq_trace:print(0, Huge),
    seq_trace:reset_trace(),
    case get_port_message(Port) of
	      {seq_trace,0,{print,_,_,[],Huge}} ->
		  ok;
	      Other2 ->
		  ct:fail({unexpected,Other2})
	  end,
    unlink(Port),
    exit(Port,kill),
    ok.

get_port_message(Port) ->
    receive
	{Port,{data,Bin}} when is_binary(Bin) ->
	    binary_to_term(Bin);
	Other ->
	    ct:fail({unexpected,Other})
    after 5000 ->
	    ct:fail(timeout)
    end.


%% OTP-15849 ERL-700
%% Verify changing label on existing token when it resides on old heap.
%% Bug caused faulty ref from old to new heap.
old_heap_token(Config) when is_list(Config) ->
    seq_trace:set_token(label, 1),
    erlang:garbage_collect(self(), [{type, minor}]),
    erlang:garbage_collect(self(), [{type, minor}]),
    %% Now token tuple should be on old-heap.
    %% Set a new non-literal label which should reside on new-heap.
    NewLabel = {self(), "new label"},
    1 = seq_trace:set_token(label, NewLabel),

    %% If bug, we now have a ref from old to new heap. Yet another minor gc
    %% will make that a ref to deallocated memory.
    erlang:garbage_collect(self(), [{type, minor}]),
    {label,NewLabel} = seq_trace:get_token(label),
    ok.


match_set_seq_token(doc) ->
    ["Tests that match spec function set_seq_token does not "
     "corrupt the heap"];
match_set_seq_token(Config) when is_list(Config) ->
    Parent = self(),

    %% OTP-4222 Match spec 'set_seq_token' corrupts heap
    %%
    %% This test crashes the emulator if the bug in question is present,
    %% it is therefore done in a slave node.
    %%
    %% All the timeout stuff is here to get decent accuracy of the error
    %% return value, instead of just 'timeout'.
    %%
    {ok, Sandbox} = start_node(seq_trace_other, []),
    true = rpc:call(Sandbox, code, add_patha,
			  [filename:dirname(code:which(?MODULE))]),
    Lbl = 4711,
    %% Do the possibly crashing test
    P1 =
	spawn(
	  fun () ->
		  Parent ! {self(),
			    rpc:call(Sandbox, 
				     ?MODULE, do_match_set_seq_token, [Lbl])}
	  end),
    %% Probe the node with a simple rpc request, to see if it is alive.
    P2 =
	spawn(
	  fun () ->
		  receive after 4000 -> ok end,
		  Parent ! {self(), rpc:call(Sandbox, erlang, abs, [-1])}
	  end),
    %% If the test node hangs completely, this timer expires.
    R3 = erlang:start_timer(8000, self(), void),
    %%
    {ok, Log} =
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

    %% Sort the log on Pid, as events from different processes
    %% are not guaranteed to arrive in a certain order to the
    %% tracer
    SortedLog = lists:keysort(2, Log),

    ok = check_match_set_seq_token_log(Lbl, SortedLog),
    %%
    stop_node(Sandbox),
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
  [{trace,B,call,{?MODULE,bounce,   [Ref]},    {0,Label,2,B,1}},
   {trace,B,call,{?MODULE,bounce,   [Ref]},    {0,Label,4,B,3}},
   {trace,B,call,{?MODULE,bounce,   [Ref]},    {0,Label,6,B,5}},
   {trace,C,call,{?MODULE,countdown,[B,Ref]},  {0,Label,0,C,0}},
   {trace,C,call,{?MODULE,countdown,[B,Ref,3]},{0,Label,0,C,0}},
   {trace,C,call,{?MODULE,countdown,[B,Ref,2]},{0,Label,2,B,1}},
   {trace,C,call,{?MODULE,countdown,[B,Ref,1]},{0,Label,4,B,3}},
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



gc_seq_token(doc) ->
    ["Tests that a seq_trace token on a message in the inqueue ",
     "can be garbage collected."];
gc_seq_token(Config) when is_list(Config) ->
    Parent = self(),

    %% OTP-4555 Seq trace token causes free mem read in gc
    %%
    %% This test crashes the emulator if the bug in question is present,
    %% it is therefore done in a slave node.
    %%
    %% All the timeout stuff is here to get decent accuracy of the error
    %% return value, instead of just 'timeout'.
    %%
    {ok, Sandbox} = start_node(seq_trace_other, []),
    true = rpc:call(Sandbox, code, add_patha,
			  [filename:dirname(code:which(?MODULE))]),
    Label = 4711,
    %% Do the possibly crashing test
    P1 =
	spawn(
	  fun () ->
 		  Parent ! {self(),
			    rpc:call(Sandbox, 
				     ?MODULE, do_gc_seq_token, [Label])}
	  end),
    %% Probe the node with a simple rpc request, to see if it is alive.
    P2 =
	spawn(
	  fun () ->
		  receive after 4000 -> ok end,
		  Parent ! {self(), rpc:call(Sandbox, erlang, abs, [-1])}
	  end),
    %% If the test node hangs completely, this timer expires.
    R3 = erlang:start_timer(8000, self(), void),
    %%
    ok =
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
    stop_node(Sandbox),
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

%% Test that messages from a port does not clear the token
port_clean_token(Config) when is_list(Config) ->
    seq_trace:reset_trace(),
    Label = make_ref(),
    seq_trace:set_token(label, Label),
    {label,Label} = seq_trace:get_token(label),

    %% Create a port and get messages from it
    %% We use os:cmd as a convenience as it does
    %% open_port, port_command, port_close and receives replies.
    %% Maybe it is not ideal to rely on the internal implementation
    %% of os:cmd but it will have to do.
    os:cmd("ls"),

    %% Make sure that the seq_trace token is still there
    {label,Label} = seq_trace:get_token(label),

    ok.


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

n_time_receiver(0) ->
    ok;
n_time_receiver(N) ->
    receive _Term -> n_time_receiver(N-1)
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
	    simple_tracer([{Label,Info, no_timestamp}|Data], DN+1);
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

set_token_flags([]) ->
    ok;
set_token_flags([no_timestamp|Flags]) ->
    seq_trace:set_token(timestamp, false),
    seq_trace:set_token(monotonic_timestamp, false),
    seq_trace:set_token(strict_monotonic_timestamp, false),
    set_token_flags(Flags);
set_token_flags([Flag|Flags]) ->
    seq_trace:set_token(Flag, true),
    set_token_flags(Flags).

check_ts(no_timestamp, Ts) ->
    try
	no_timestamp = Ts
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok;
check_ts(timestamp, Ts) ->
    try
	{Ms,S,Us} = Ts,
	true = is_integer(Ms),
	true = is_integer(S),
	true = is_integer(Us)
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok;
check_ts(monotonic_timestamp, Ts) ->
    try
	true = is_integer(Ts)
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok;
check_ts(strict_monotonic_timestamp, Ts) ->
    try
	{MT, UMI} = Ts,
	true = is_integer(MT),
	true = is_integer(UMI)
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok.

start_node(Name, Param) ->
    test_server:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    test_server:stop_node(Node).

load_tracer(Config) ->
    Path = proplists:get_value(data_dir, Config),
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
