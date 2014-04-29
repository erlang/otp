%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2014. All Rights Reserved.
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

-module(match_spec_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, not_run/1]).
-export([test_1/1, test_2/1, test_3/1, bad_match_spec_bin/1,
	 trace_control_word/1, silent/1, silent_no_ms/1, silent_test/1,
	 ms_trace2/1, ms_trace3/1, boxed_and_small/1,
	 destructive_in_test_bif/1, guard_exceptions/1,
	 empty_list/1,
	 unary_plus/1, unary_minus/1, moving_labels/1]).
-export([fpe/1]).
-export([otp_9422/1]).
-export([faulty_seq_trace/1, do_faulty_seq_trace/0]).
-export([runner/2, loop_runner/3]).
-export([f1/1, f2/2, f3/2, fn/1, fn/2, fn/3]).
-export([do_boxed_and_small/0]).

% This test suite assumes that tracing in general works. What we test is
% the match spec functionality.

-include_lib("test_server/include/test_server.hrl").

-export([init_per_testcase/2, end_per_testcase/2]).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:seconds(30)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case test_server:is_native(match_spec_SUITE) of
	false ->
	    [test_1, test_2, test_3, bad_match_spec_bin,
	     trace_control_word, silent, silent_no_ms, silent_test, ms_trace2,
	     ms_trace3, boxed_and_small, destructive_in_test_bif,
	     guard_exceptions, unary_plus, unary_minus, fpe,
	     moving_labels,
	     faulty_seq_trace,
	     empty_list,
	     otp_9422];
	true -> [not_run]
    end.

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


not_run(Config) when is_list(Config) ->
    {skipped, "Native Code"}.

test_1(doc) ->
    [""];
test_1(suite) -> [];
test_1(Config) when is_list(Config) ->
    ?line tr(fun() -> ?MODULE:f1(a) end,
	     {?MODULE, f1, 1},
	     [],
	     [{call, {?MODULE, f1, [a]}}]),

    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$1','$1'],[{is_atom, '$1'}],[]}],
	     [{call, {?MODULE, f2, [a, a]}}]),

    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$1','$1'],[{is_atom, '$1'}],[{message, false}]}],
	     []),

    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$1','$1'],[{is_atom, '$1'}],[{message, 4711}]}],
	     [{call, {?MODULE, f2, [a, a]}, 4711}]),

    Ref = make_ref(),
    ?line tr(fun() -> ?MODULE:f2(Ref, Ref) end,
	     {?MODULE, f2, 2},
	     [{[Ref,'$1'],[{is_reference, '$1'}],[{message, 4711}]}],
	     [{call, {?MODULE, f2, [Ref, Ref]}, 4711}]),
    ?line tr(fun() -> ?MODULE:f2(Ref, Ref) end,
	     {?MODULE, f2, 2},
	     [{['$1',Ref],[{is_reference, '$1'}],[{message, 4711}]}],
	     [{call, {?MODULE, f2, [Ref, Ref]}, 4711}]),

    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$0','$0'],[{is_atom, '$0'}],[{message, 4711}]}],
	     [{call, {?MODULE, f2, [a, a]}, 4711}]),

    ?line tr(fun() -> ?MODULE:f2(a, b) end,
	     {?MODULE, f2, 2},
	     [{['_','_'],[],[]}],
	     [{call, {?MODULE, f2, [a, b]}}]),

    ?line tr(fun() -> ?MODULE:f2(a, b) end,
	     {?MODULE, f2, 2},
	     [{['_','_'],[],[{message, '$_'}]}],
	     [{call, {?MODULE, f2, [a, b]}, [a, b]}]),

    ?line tr(fun() -> ?MODULE:f2(a, '$_') end,
	     {?MODULE, f2, 2},
	     [{['$1','$_'],[{is_atom, '$1'}],[]}],
	     [{call, {?MODULE, f2, [a, '$_']}}]),

    ?line tr(fun() -> ?MODULE:f1({a}) end,
	     {?MODULE, f1, 1},
	     [{['$1'],[{'==', '$1', {const, {a}}}],[]}],
	     [{call, {?MODULE, f1, [{a}]}}]),

    ?line tr(fun() -> ?MODULE:f1({a}) end,
	     {?MODULE, f1, 1},
	     [{['$1'],[{'==', '$1', {{a}}}],[]}],
	     [{call, {?MODULE, f1, [{a}]}}]),

%% Undocumented, currently.
    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$1','$1'],[{is_atom, '$1'}],[{message, 4711},
					      {message, true}]}],
	     [{call, {?MODULE, f2, [a, a]}}]),

    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$1','$1'],[{is_atom, '$1'}],[{message, 4711},
					      {message, false}]}],
	     []),

    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$1','$1'],[{is_atom, '$1'}],[kakalorum]}],
	     [{call, {?MODULE, f2, [a, a]}}]),

%    case tr0(fun() -> ?MODULE:f2(a, a) end,
%	     {?MODULE, f2, 2},
%	     [{['$1','$1'],[{is_atom, '$1'}],[{message, {process_dump}}]}]) of
%	[{trace, _, call, {?MODULE, f2, [a, a]}, Bin}] ->
%	    erlang:display(binary_to_list(Bin))
%    end,

% Error cases
    ?line errchk([{['$1','$1'],[{is_atom, '$1'}],[{banka, kanin}]}]),

    ok.

test_2(doc) ->
    [""];
test_2(suite) -> [];
test_2(Config) when is_list(Config) ->
    ?line tr(fun() -> ?MODULE:f2(a, a) end,
	     {?MODULE, f2, 2},
	     [{['$1','$1'],[{is_atom, '$1'}],[{return_trace}]}],
	     [{call, {?MODULE, f2, [a, a]}},
	      {return_from, {?MODULE, f2, 2}, {a, a}}]),
    ok.

test_3(doc) ->
    ["Test the enable_trace/2 and caller/0 PAM instructions"];
test_3(suite) -> [];
test_3(Config) when is_list(Config) ->
    ?line Fun1 = fun() -> 
		   register(fnoppelklopfer,self()),
		   ?MODULE:f2(a, b),
		   ?MODULE:f2(a, b) 
	   end,
    ?line P1 = spawn(?MODULE, runner, [self(), Fun1]),
    ?line Pat = [{['$1','$1'],[],[{message,
				   [{enable_trace, P1, call},{caller}]}]},
		 {['_','_'],[],[{message,
				 [{disable_trace, fnoppelklopfer, call}]}]}],
    ?line Fun2 = fun() -> ?MODULE:f3(a, a) end,
    ?line P2 = spawn(?MODULE, runner, [self(), Fun2]),
    ?line erlang:trace(P2, true, [call]),
    ?line erlang:trace_pattern({?MODULE, f2, 2}, Pat),
    ?line collect(P2, [{trace, P2, call, {?MODULE, f2, [a, a]}, [true,
							     {?MODULE,f3,2}]}]),
    ?line collect(P1, [{trace, P1, call, {?MODULE, f2, [a, b]}, [true]}]),
    ?line ok.

otp_9422(doc) -> [];
otp_9422(Config) when is_list(Config) ->
    Laps = 10000,
    ?line Fun1 = fun() -> otp_9422_tracee() end,
    ?line P1 = spawn_link(?MODULE, loop_runner, [self(), Fun1, Laps]),
    io:format("spawned ~p as tracee\n", [P1]),

    ?line erlang:trace(P1, true, [call, silent]),

    ?line Fun2 = fun() -> otp_9422_trace_changer() end,
    ?line P2 = spawn_link(?MODULE, loop_runner, [self(), Fun2, Laps]),
    io:format("spawned ~p as trace_changer\n", [P2]),

    start_collect(P1),
    start_collect(P2),

    %%receive after 10*1000 -> ok end,

    stop_collect(P1),
    stop_collect(P2, abort),
    ok.
    
otp_9422_tracee() ->
    ?MODULE:f1(a),
    ?MODULE:f1(b),
    ?MODULE:f1(c).

otp_9422_trace_changer() ->
    Pat1 = [{[a], [], [{enable_trace, arity}]}],
    ?line erlang:trace_pattern({?MODULE, f1, 1}, Pat1),
    Pat2 = [{[b], [], [{disable_trace, arity}]}],
    ?line erlang:trace_pattern({?MODULE, f1, 1}, Pat2).

    
    


bad_match_spec_bin(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch ets:match_spec_run([1], <<>>)),
    B0 = <<1,2>>,
    {B,_} = split_binary(B0, 0),
    {'EXIT',{badarg,_}} = (catch ets:match_spec_run([1], B)),
    ok.



trace_control_word(doc) ->
    ["Test the erlang:system_info(trace_control_word) and ",
     "erlang:system_flag(trace_control_word, Value) BIFs, ", 
     "as well as the get_tcw/0 and set_tcw/1 PAM instructions"];
trace_control_word(suite) -> [];
trace_control_word(Config) when is_list(Config) ->
    ?line 32 = Bits = tcw_bits(),
    ?line High = 1 bsl (Bits - 1),
    ?line erlang:system_flag(trace_control_word, 17),
    ?line tr(fun() -> ?MODULE:f1(a) end,
	     {?MODULE, f1, 1},
	     [{'_',[{'=:=', {get_tcw}, 17}],[]}],
	     [{call, {?MODULE, f1, [a]}}]),
    ?line tr(fun() -> ?MODULE:f1(a) end,
	     {?MODULE, f1, 1},
	     [{'_',[{'=:=', {get_tcw}, 18}],[]}],
	     []),
    ?line erlang:system_flag(trace_control_word, High),
    ?line tr(fun() -> ?MODULE:f1(a) end,
	     {?MODULE, f1, 1},
	     [{'_',[{'=:=', {get_tcw}, High}],[]}],
	     [{call, {?MODULE, f1, [a]}}]),
    ?line erlang:system_flag(trace_control_word, 0),
    ?line tr(fun() -> 
		     ?MODULE:f1(a), 
		     ?MODULE:f1(start), 
		     ?MODULE:f1(b), 
		     ?MODULE:f1(c), 
		     ?MODULE:f1(high), 
		     ?MODULE:f1(d),
		     ?MODULE:f1(stop), 
		     ?MODULE:f1(e) 
	     end,
	     {?MODULE, f1, 1},
	     [{[start],
	       [],
	       [{message, {set_tcw, 17}}]},
	      {[stop],
	       [],
	       [{message, {set_tcw, 0}}]},
	      {[high],
	       [],
	       [{message, {set_tcw, High}}]},
	      {['_'],
	       [{'>', {get_tcw}, 0}],
	       [{set_tcw, {'+', 1, {get_tcw}}}, {message, {get_tcw}}] }],
	     [{call, {?MODULE, f1, [start]}, 0},
	      {call, {?MODULE, f1, [b]}, 18},
	      {call, {?MODULE, f1, [c]}, 19},
	      {call, {?MODULE, f1, [high]}, 19},
	      {call, {?MODULE, f1, [d]}, High + 1},
	      {call, {?MODULE, f1, [stop]}, High + 1}]),
    ?line 0 = erlang:system_info(trace_control_word),
    ok.

tcw_bits() ->
    ?line tcw_bits(erlang:system_flag(trace_control_word, 0), 0, 0).

tcw_bits(Save, Prev, Bits) ->
    ?line Curr = 1 bsl Bits,
    ?line case catch erlang:system_flag(trace_control_word, Curr) of
	      {'EXIT' , {badarg, _}} ->
		  ?line Prev = erlang:system_flag(trace_control_word, Save),
		  Bits;
	      Prev ->
		  ?line Curr = erlang:system_info(trace_control_word),
		  tcw_bits(Save, Curr, Bits+1)
	  end.



silent(doc) ->
    ["Test the erlang:trace(_, _, [silent]) flag ",
     "as well as the silent/0 PAM instruction"];
silent(suite) -> [];
silent(Config) when is_list(Config) ->
    %% Global call trace
    ?line tr(fun() -> 
		     ?MODULE:f1(a),     % No trace - not active
		     ?MODULE:f1(miss),  % No trace - no activation
		     ?MODULE:f1(b),     % No trace - still not active
		     ?MODULE:f1(start), % Trace    - activation
		     ?MODULE:f1(c),     % Trace    - active
		     f1(d),             % No trace - local call
		     ?MODULE:f1(miss),  % Trace    - no inactivation
		     ?MODULE:f1(e),     % Trace    - still active
		     ?MODULE:f1(stop),  % No trace - inactivation
		     ?MODULE:f1(f)      % No trace - not active
	     end,
	     {?MODULE, f1, 1},
	     [call, silent],
	     [{[start],
	       [],
	       [{silent, false}, {message, start}]},
	      {[stop],
	       [],
	       [{silent, true}, {message, stop}]},
	      {[miss],
	       [],
	       [{silent, neither_true_nor_false}, {message, miss}]},
	      {['$1'],
	       [],
	       [{message, '$1'}] }],
	     [global],
	     [{call, {?MODULE, f1, [start]}, start},
	      {call, {?MODULE, f1, [c]}, c},
	      {call, {?MODULE, f1, [miss]}, miss},
	      {call, {?MODULE, f1, [e]}, e} ]),
    %% Local call trace
    ?line tr(fun() -> 
		     ?MODULE:f1(a),     % No trace - not active
		     f1(b),             % No trace - not active
		     ?MODULE:f1(start), % Trace    - activation
		     ?MODULE:f1(c),     % Trace    - active
		     f1(d),             % Trace    - active
		     f1(stop),          % No trace - inactivation
		     ?MODULE:f1(e),     % No trace - not active
		     f1(f)              % No trace - not active
	     end,
	     {?MODULE, f1, 1},
	     [call, silent],
	     [{[start],
	       [],
	       [{silent, false}, {message, start}]},
	      {[stop],
	       [],
	       [{silent, true}, {message, stop}]},
	      {['$1'],
	       [],
	       [{message, '$1'}] }],
	     [local],
	     [{call, {?MODULE, f1, [start]}, start},
	      {call, {?MODULE, f1, [c]}, c},
	      {call, {?MODULE, f1, [d]}, d} ]),
    ok.

silent_no_ms(doc) ->
    ["Test the erlang:trace(_, _, [silent]) flag without match specs"];
silent_no_ms(suite) -> [];
silent_no_ms(Config) when is_list(Config) ->
    %% Global call trace
    %%
    %% Trace f2/2 and erlang:integer_to_list/1 without match spec
    %% and use match spec on f1/1 to control silent flag.
    ?line tr(
	    fun () -> 
		    ?MODULE:f1(a),
		    ?MODULE:f2(b, c),
		    _ = erlang:integer_to_list(id(1)),
		    ?MODULE:f3(d, e),
		    ?MODULE:f1(start),
		    ?MODULE:f2(f, g),
		    _ = erlang:integer_to_list(id(2)),
		    ?MODULE:f3(h, i),
		    ?MODULE:f1(stop),
		    ?MODULE:f2(j, k),
		    _ = erlang:integer_to_list(id(3)),
		    ?MODULE:f3(l, m)
	    end,
	    fun (Tracee) ->
		    ?line 1 = 
			erlang:trace(Tracee, true, 
				     [call,silent,return_to]),
		    ?line 1 =
			erlang:trace_pattern(
			  {?MODULE,f2,2}, 
			  [], 
			  [global]),
		    ?line 1 = 
			erlang:trace_pattern(
			  {erlang,integer_to_list,1}, 
			  [], 
			  [global]),
		    ?line 1 = 
			erlang:trace_pattern(
			  {?MODULE,f1,1},
			  [{[start],[],[{silent,false}]},
			   {[stop],[],[{silent,true}]}],
			  [global]),
		    %%
		    %% Expected: (no return_to for global call trace)
		    %%
		    ?line
			[{trace,Tracee,call,{?MODULE,f1,[start]}},
			 {trace,Tracee,call,{?MODULE,f2,[f,g]}},
		         {trace,Tracee,call,{erlang,integer_to_list,[2]}},
			 {trace,Tracee,call,{?MODULE,f2,[h,i]}}]
	    end),
    %% Local call trace
    %%
    %% Trace f2/2 and erlang:integer_to_list/1 without match spec
    %% and use match spec on f1/1 to control silent flag.
    ?line tr(
	    fun () -> 
		    ?MODULE:f1(a),
		    ?MODULE:f2(b, c),
		    _ = erlang:integer_to_list(id(1)),
		    ?MODULE:f3(d, e),
		    ?MODULE:f1(start),
		    ?MODULE:f2(f, g),
		    _ = erlang:integer_to_list(id(2)),
		    ?MODULE:f3(h, i),
		    ?MODULE:f1(stop),
		    ?MODULE:f2(j, k),
		    _ = erlang:integer_to_list(id(3)),
		    ?MODULE:f3(l, m)
	    end,
	    fun (Tracee) ->
		    ?line 1 =
			erlang:trace(Tracee, true, 
				     [call,silent,return_to]),
		    ?line 1 = 
			erlang:trace_pattern(
			  {?MODULE,f2,2}, 
			  [], 
			  [local]),
		    ?line 1 = 
			erlang:trace_pattern(
			  {erlang,integer_to_list,1}, 
			  [], 
			  [local]),
		    ?line 1 =
			erlang:trace_pattern(
			  {?MODULE,f1,1},
			  [{[start],[],[{silent,false}]},
			   {[stop],[],[{silent,true}]}],
			  [local]),
		    %%
		    %% Expected:
		    %%
		    ?line
			[{trace,Tracee,call,{?MODULE,f1,[start]}},
			 {trace,Tracee,return_to,
			  {?MODULE,'-silent_no_ms/1-fun-2-',0}},
			 {trace,Tracee,call,{?MODULE,f2,[f,g]}},
			 {trace,Tracee,return_to,
			  {?MODULE,'-silent_no_ms/1-fun-2-',0}},
		         {trace,Tracee,call,{erlang,integer_to_list,[2]}},
			 {trace,Tracee,return_to,
			  {?MODULE,'-silent_no_ms/1-fun-2-',0}},
			 {trace,Tracee,call,{?MODULE,f2,[h,i]}},
			 {trace,Tracee,return_to,{?MODULE,f3,2}}]
	    end).

silent_test(doc) ->
    ["Test that match_spec_test does not activate silent"];
silent_test(_Config) ->
    {flags,[]} = erlang:trace_info(self(),flags),
    erlang:match_spec_test([],[{'_',[],[{silent,true}]}],trace),
    {flags,[]} = erlang:trace_info(self(),flags).


ms_trace2(doc) ->
    ["Test the match spec functions {trace/2}"];
ms_trace2(suite) -> [];
ms_trace2(Config) when is_list(Config) ->
    Tracer = self(),
    %% Meta trace init
    %%
    %% Trace global f1/1, local f2/2, global erlang:integer_to_list/1
    %% without match spec. Use match spec functions
    %% {trace/2} to control trace through fn/2,3.
    ?line tr(
	    fun () -> 
		    ?MODULE:f1(a),
		    ?MODULE:f2(b, c),
		    _ = erlang:integer_to_list(id(1)),
		    ?MODULE:f3(d, e),
		    fn([all], [call,return_to,{tracer,Tracer}]),
		    ?MODULE:f1(f),
		    f2(g, h),
		    f1(i),
		    _ = erlang:integer_to_list(id(2)),
		    ?MODULE:f3(j, k),
		    fn([call,return_to], []),
		    ?MODULE:f1(l),
		    ?MODULE:f2(m, n),
		    _ = erlang:integer_to_list(id(3)),
		    ?MODULE:f3(o, p)
	    end,
	    fun (Tracee) ->
		    ?line 1 = 
			erlang:trace(Tracee, false, [all]),
		    ?line 1 =
			erlang:trace_pattern(
			  {?MODULE,f1,1}, 
			  [], 
			  [global]),
		    ?line 1 =
			erlang:trace_pattern(
			  {?MODULE,f2,2}, 
			  [], 
			  [local]),
		    ?line 1 = 
			erlang:trace_pattern(
			  {erlang,integer_to_list,1}, 
			  [], 
			  [global]),
		    ?line 3 = 
			erlang:trace_pattern(
			  {?MODULE,fn,'_'},
			  [{['$1','$2'],[],
			    [{trace,'$1','$2'},{message,ms_trace2}]}],
			  [meta]),
		    %%
		    %% Expected: (no return_to for global call trace)
		    %%
		    ?line Origin = {match_spec_SUITE,'-ms_trace2/1-fun-0-',1},
		    ?line
			[{trace_ts,Tracee,call,
			  {?MODULE,fn,
			   [[all],[call,return_to,{tracer,Tracer}]]},
			  ms_trace2},
			 {trace,Tracee,call,{?MODULE,f1,[f]}},
			 {trace,Tracee,call,{?MODULE,f2,[g,h]}},
			 {trace,Tracee,return_to,Origin},
		         {trace,Tracee,call,{erlang,integer_to_list,[2]}},
			 {trace,Tracee,call,{?MODULE,f2,[j,k]}},
			 {trace,Tracee,return_to,{?MODULE,f3,2}},
			 {trace_ts,Tracee,call,
			  {?MODULE,fn,
			   [[call,return_to],[]]},
			  ms_trace2}]
	    end),
    ok.



ms_trace3(doc) ->
    ["Test the match spec functions {trace/3}"];
ms_trace3(suite) -> [];
ms_trace3(Config) when is_list(Config) ->
    TraceeName = 'match_spec_SUITE:ms_trace3',
    Tracer = self(),
    %% Meta trace init
    %%
    %% Trace global f1/1, local f2/2, global erlang:integer_to_list/1
    %% without match spec. Use match spec functions
    %% {trace/2} to control trace through fn/2,3.
    Tag = make_ref(),
    Controller =
	spawn_link(
	  fun () ->
		  receive 
		      {Tracee,Tag,start} ->
			  fn(TraceeName, [all], 
			     [call,return_to,send,'receive',
			      {tracer,Tracer}]),
			  Tracee ! {self(),Tag,started},
			  receive {Tracee,Tag,stop_1} -> ok end,
			  fn(Tracee, [call,return_to], []),
			  Tracee ! {self(),Tag,stopped_1},
			  receive {Tracee,Tag,stop_2} -> ok end,
			  fn(Tracee, [all], []),
			  Tracee ! {self(),Tag,stopped_2}
		  end
	  end),
    ?line tr(
	    fun () -> %% Action
		    register(TraceeName, self()),
		    ?MODULE:f1(a),
		    ?MODULE:f2(b, c),
		    _ = erlang:integer_to_list(id(1)),
		    ?MODULE:f3(d, e),
		    Controller ! {self(),Tag,start},
		    receive {Controller,Tag,started} -> ok end,
		    ?MODULE:f1(f),
		    f2(g, h),
		    f1(i),
		    _ = erlang:integer_to_list(id(2)),
		    ?MODULE:f3(j, k),
		    Controller ! {self(),Tag,stop_1},
		    receive {Controller,Tag,stopped_1} -> ok end,
		    ?MODULE:f1(l),
		    ?MODULE:f2(m, n),
		    _ = erlang:integer_to_list(id(3)),
		    ?MODULE:f3(o, p),
		    Controller ! {self(),Tag,stop_2},
		    receive {Controller,Tag,stopped_2} -> ok end,
		    ?MODULE:f1(q),
		    ?MODULE:f2(r, s),
		    _ = erlang:integer_to_list(id(4)),
		    ?MODULE:f3(t, u)
	    end,
	    
	    fun (Tracee) -> %% Startup
		    ?line 1 = 
			erlang:trace(Tracee, false, [all]),
		    ?line 1 =
			erlang:trace_pattern(
			  {?MODULE,f1,1}, 
			  [], 
			  [global]),
		    ?line 1 =
			erlang:trace_pattern(
			  {?MODULE,f2,2}, 
			  [], 
			  [local]),
		    ?line 1 = 
			erlang:trace_pattern(
			  {erlang,integer_to_list,1}, 
			  [], 
			  [global]),
		    ?line 3 = 
			erlang:trace_pattern(
			  {?MODULE,fn,'_'},
			  [{['$1','$2','$3'],[],
			    [{trace,'$1','$2','$3'},{message,Tag}]}],
			  [meta]),
		    %%
		    %% Expected: (no return_to for global call trace)
		    %%
		    ?line Origin = {match_spec_SUITE,'-ms_trace3/1-fun-1-',2},
		    ?line
			[{trace_ts,Controller,call,
			  {?MODULE,fn,[TraceeName,[all],
				       [call,return_to,send,'receive',
					{tracer,Tracer}]]},
			  Tag},
			 {trace,Tracee,'receive',{Controller,Tag,started}},
			 {trace,Tracee,call,{?MODULE,f1,[f]}},
			 {trace,Tracee,call,{?MODULE,f2,[g,h]}},
			 {trace,Tracee,return_to,Origin},
		         {trace,Tracee,call,{erlang,integer_to_list,[2]}},
			 {trace,Tracee,call,{?MODULE,f2,[j,k]}},
			 {trace,Tracee,return_to,{?MODULE,f3,2}},
			 {trace,Tracee,send,{Tracee,Tag,stop_1},Controller},
			 {trace_ts,Controller,call,
			  {?MODULE,fn,[Tracee,[call,return_to],[]]},
			  Tag},
			 {trace_ts,Controller,call,
			  {?MODULE,fn,[Tracee,[all],[]]},
			  Tag}]
	    end),
    ok.



destructive_in_test_bif(doc) ->
    ["Test that destructive operations in test bif does not really happen"];
destructive_in_test_bif(suite) -> [];
destructive_in_test_bif(Config) when is_list(Config) ->
    ?line {ok,OldToken,_,_} = erlang:match_spec_test
				([],
				 [{'_',[],[{message,{get_seq_token}}]}],trace),
    ?line {ok,_,_,_} = erlang:match_spec_test
			 ([],
			  [{'_',[],[{message,{set_seq_token, label, 1}}]}],
			  trace),
    ?line {ok,OldToken,_,_} = erlang:match_spec_test
				([],
				 [{'_',[],[{message,{get_seq_token}}]}],trace),
    ?line {ok, OldTCW,_,_} = erlang:match_spec_test
			       ([],[{'_',[],[{message,{get_tcw}}]}],trace),
    ?line {ok,OldTCW,_,_} = erlang:match_spec_test
			      ([],
			       [{'_',[],[{message,{set_tcw, OldTCW+1}}]}],
			       trace),
    ?line {ok, OldTCW,_,_} = erlang:match_spec_test
			       ([],[{'_',[],[{message,{get_tcw}}]}],trace),
    ok.

boxed_and_small(doc) ->
    ["Test that the comparision between boxed and small does not crash emulator"];
boxed_and_small(suite) -> [];
boxed_and_small(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(match_spec_suite_other),
    ?line ok = rpc:call(Node,?MODULE,do_boxed_and_small,[]),
    ?line stop_node(Node),
    ok.

do_boxed_and_small() ->
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{1.47,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{12345678901234567890,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{<<1,2,3,4>>,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{make_ref(),'_'},[],['$_']}],table),
    ok.

faulty_seq_trace(doc) ->
    ["Test that faulty seq_trace_call does not crash emulator"];
faulty_seq_trace(suite) -> [];
faulty_seq_trace(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(match_spec_suite_other),
    ?line ok = rpc:call(Node,?MODULE,do_faulty_seq_trace,[]),
    ?line stop_node(Node),
    ok.

do_faulty_seq_trace() ->
    {ok,'EXIT',_,_} = erlang:match_spec_test([],[{'_',[],[{message,{set_seq_token,yxa,true}}]}],trace),
    ok.

errchk(Pat) ->
    case catch erlang:trace_pattern({?MODULE, f2, 2}, Pat) of
	{'EXIT', {badarg, _}} ->
	    ok;
	Other ->
	    test_server:fail({noerror, Other})
    end.

unary_minus(suite) ->
    [];
unary_minus(doc) ->
    ["Checks that unary minus works"];
unary_minus(Config) when is_list(Config) ->
    ?line {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'-','$1'},-4}],
				 [true]}],
			       table),
    ?line {ok,false,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'-','$1'},-6}],
				 [true]}],
			       table),
    ?line {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'=:=',{'-','$1',2},3}],
				 [true]}],
			       table),
    ?line {ok,false,[],[]} = erlang:match_spec_test
			      (hej,
			       [{'$1',
				 [{'=/=',{'-','$1'},0}],
				 [true]}],
			       table),
    ok.
unary_plus(suite) ->
    [];
unary_plus(doc) ->
    ["Checks that unary plus works"];
unary_plus(Config) when is_list(Config) ->
    ?line {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'+','$1'},6}],
				 [true]}],
			       table),
    ?line {ok,false,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'+','$1'},4}],
				 [true]}],
			       table),
    ?line {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'=:=',{'+','$1',2},7}],
				 [true]}],
			       table),
    ?line {ok,false,[],[]} = erlang:match_spec_test
			      (hej,
			       [{'$1',
				 [{'=/=',{'+','$1'},0}],
				 [true]}],
			       table),
    ok.


    

guard_exceptions(suite) ->
    [];
guard_exceptions(doc) ->
    ["Checks that exceptions in guards are handled correctly"];
guard_exceptions(Config) when is_list(Config) ->
    ?line {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},{'or','$1','$1'}}],
				  [true]}],
				table),
    ?line {ok,true,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'orelse',{is_integer,'$1'},
				    {'or','$1','$1'}}],
				  [true]}],
				table),
    ?line {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'orelse',{'or','$1',true},
				    {is_integer,'$1'}}],
				  [true]}],
				table),
    ?line {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'orelse','$1',true}}],
				  [true]}],
				table),
    ?line {ok,true,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'orelse',true,'$1'}}],
				  [true]}],
				table),
    ?line {ok,true,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'andalso',false,'$1'}}],
				  [true]}],
				table),
    ?line {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'andalso','$1',false}}],
				  [true]}],
				table),

    ?line {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'andalso','$1',false}}],
				  [true]}],
				table),

    ok.

fpe(suite) ->
    [];
fpe(doc) ->
    ["Checks floating point exceptions in match-specs"];
fpe(Config) when is_list(Config) ->
    MS = [{{'$1'},[],[{'/','$1',0}]}],
    case catch (['EXIT','EXIT'] = 
		ets:match_spec_run([{1},{2}],ets:match_spec_compile(MS))) of 
	{'EXIT',_} -> test_server:fail({error, 
					"Floating point exceptions faulty"});
	_ -> ok 
    end.

empty_list(Config) when is_list(Config) ->
    Val=[{'$1',[], [{message,'$1'},{message,{caller}},{return_trace}]}],
     %% Did crash debug VM in faulty assert:
    erlang:match_spec_test([],Val,trace).

moving_labels(Config) when is_list(Config) ->
    %% Force an andalso/orelse construction to be moved by placing it
    %% in a tuple followed by a constant term. Labels should still
    %% point at their correct target.
    %% 
    Ms = [{{'$1','$2'},[],[{{ok,{'andalso','$1','$2'},[1,2,3]}}]}],
    ?line {ok,{ok,false,[1,2,3]},[],[]} =
	erlang:match_spec_test({true,false}, Ms, table),

    Ms2 = [{{'$1','$2'},[],[{{ok,{'orelse','$1','$2'},[1,2,3]}}]}],
    ?line {ok,{ok,true,[1,2,3]},[],[]} =
	erlang:match_spec_test({true,false}, Ms2, table),

    ok.
    
tr(Fun, MFA, Pat, Expected) ->
    tr(Fun, MFA, [call], Pat, [global], Expected).

tr(Fun, MFA, TraceFlags, Pat, PatFlags, Expected0) ->
    tr(Fun,
       fun(P) ->
	       erlang:trace(P, true, TraceFlags),
	       erlang:trace_pattern(MFA, Pat, PatFlags),
	       lists:map(
		 fun(X) -> 
			 list_to_tuple([trace, P | tuple_to_list(X)])
		 end,
		 Expected0)
       end).

tr(RunFun, ControlFun) ->
    P = spawn(?MODULE, runner, [self(), RunFun]),
    collect(P, ControlFun(P)).

collect(P, TMs) ->
    start_collect(P),
    collect(TMs),
    stop_collect(P).

collect([]) ->
    receive
	M ->
	    ?t:format("Got unexpected: ~p~n", [M]),
	    flush({got_unexpected,M})
    after 17 ->
	    ok
    end;
collect([TM | TMs]) ->
    ?t:format(        "Expecting:      ~p~n", [TM]),
    receive
	M ->
	    case if element(1, M) == trace_ts ->
			 list_to_tuple(lists:reverse(
					 tl(lists:reverse(tuple_to_list(M)))));
		    true -> M
		 end of
		TM ->
		    ?t:format("Got:            ~p~n", [M]),
		    collect(TMs);
		_ ->
		    ?t:format("Got unexpected: ~p~n", [M]),
		    flush({got_unexpected,M})
	    end
    end.

flush(Reason) ->
    receive
	M ->
	    ?t:format("In queue:       ~p~n", [M]),
	    flush(Reason)
    after 17 ->
	    ?t:fail(Reason)
    end.

start_collect(P) ->
    P ! {go, self()}.

stop_collect(P) ->
    stop_collect(P, done).
stop_collect(P, Order) ->
    P ! {Order, self()},
    receive
	{gone, P} ->
	    ok
    end.


runner(Collector, Fun) ->
    receive
	{go, Collector} ->
	    go
    end,
    Fun(),
    receive
	{done, Collector} ->
	    Collector ! {gone, self()}
    end.

loop_runner(Collector, Fun, Laps) ->
    receive
	{go, Collector} ->
	    go
    end,
    loop_runner_cont(Collector, Fun, 0, Laps).

loop_runner_cont(Collector, _Fun, Laps, Laps) ->
    receive
	{done, Collector} -> ok;
	{abort, Collector} -> ok
    end,
    io:format("loop_runner ~p exit after ~p laps\n", [self(), Laps]),
    Collector ! {gone, self()};

loop_runner_cont(Collector, Fun, N, Laps) ->
    Fun(),
    receive
	{abort, Collector} ->
	    io:format("loop_runner ~p aborted after ~p of ~p laps\n", [self(), N+1, Laps]),
	    Collector ! {gone, self()}
    after 0 ->
	    loop_runner_cont(Collector, Fun, N+1, Laps)
    end.


f1(X) ->
    {X}.

f2(X, Y) ->
    {X, Y}.

f3(X,Y) ->
    ?MODULE:f2(X,Y),
    ok.

fn(X) ->
    [X].
fn(X, Y) ->
    [X, Y].
fn(X, Y, Z) ->
    [X, Y, Z].

id(X) ->
    X.

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = atom_to_list(erlang:get_cookie()),
    test_server:start_node(Name, slave, 
			   [{args, "-setcookie " ++ Cookie ++" -pa " ++ Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).
