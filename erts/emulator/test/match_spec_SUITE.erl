%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

-module(match_spec_SUITE).

-export([all/0, suite/0, not_run/1]).
-export([test_1/1, test_2/1, test_3/1, caller_and_return_to/1, bad_match_spec_bin/1,
	 trace_control_word/1, silent/1, silent_no_ms/1, silent_test/1,
	 ms_trace2/1, ms_trace3/1, ms_trace_dead/1, boxed_and_small/1,
	 destructive_in_test_bif/1, guard_exceptions/1,
	 empty_list/1,
	 unary_plus/1, unary_minus/1, moving_labels/1]).
-export([fpe/1]).
-export([otp_9422/1]).
-export([faulty_seq_trace/1, do_faulty_seq_trace/0]).
-export([maps/1]).
-export([runner/2, loop_runner/3]).
-export([f1/1, f2/2, f3/2, fn/1, fn/2, fn/3]).
-export([do_boxed_and_small/0]).

% This test suite assumes that tracing in general works. What we test is
% the match spec functionality.

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    case test_server:is_native(match_spec_SUITE) of
	false ->
	    [test_1, test_2, test_3, caller_and_return_to, bad_match_spec_bin,
	     trace_control_word, silent, silent_no_ms, silent_test, ms_trace2,
	     ms_trace3, ms_trace_dead, boxed_and_small, destructive_in_test_bif,
	     guard_exceptions, unary_plus, unary_minus, fpe,
	     moving_labels,
	     faulty_seq_trace,
	     empty_list,
             otp_9422,
             maps];
	true -> [not_run]
    end.

not_run(Config) when is_list(Config) ->
    {skipped, "Native Code"}.

test_1(Config) when is_list(Config) ->
    tr(fun() -> ?MODULE:f1(a) end,
       {?MODULE, f1, 1},
       [],
       [{call, {?MODULE, f1, [a]}}]),

    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$1','$1'],[{is_atom, '$1'}],[]}],
       [{call, {?MODULE, f2, [a, a]}}]),

    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$1','$1'],[{is_atom, '$1'}],[{message, false}]}],
       []),

    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$1','$1'],[{is_atom, '$1'}],[{message, 4711}]}],
       [{call, {?MODULE, f2, [a, a]}, 4711}]),

    Ref = make_ref(),
    tr(fun() -> ?MODULE:f2(Ref, Ref) end,
       {?MODULE, f2, 2},
       [{[Ref,'$1'],[{is_reference, '$1'}],[{message, 4711}]}],
       [{call, {?MODULE, f2, [Ref, Ref]}, 4711}]),
    tr(fun() -> ?MODULE:f2(Ref, Ref) end,
       {?MODULE, f2, 2},
       [{['$1',Ref],[{is_reference, '$1'}],[{message, 4711}]}],
       [{call, {?MODULE, f2, [Ref, Ref]}, 4711}]),

    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$0','$0'],[{is_atom, '$0'}],[{message, 4711}]}],
       [{call, {?MODULE, f2, [a, a]}, 4711}]),

    tr(fun() -> ?MODULE:f2(a, b) end,
       {?MODULE, f2, 2},
       [{['_','_'],[],[]}],
       [{call, {?MODULE, f2, [a, b]}}]),

    tr(fun() -> ?MODULE:f2(a, b) end,
       {?MODULE, f2, 2},
       [{['_','_'],[],[{message, '$_'}]}],
       [{call, {?MODULE, f2, [a, b]}, [a, b]}]),

    tr(fun() -> ?MODULE:f2(a, '$_') end,
       {?MODULE, f2, 2},
       [{['$1','$_'],[{is_atom, '$1'}],[]}],
       [{call, {?MODULE, f2, [a, '$_']}}]),

    tr(fun() -> ?MODULE:f1({a}) end,
       {?MODULE, f1, 1},
       [{['$1'],[{'==', '$1', {const, {a}}}],[]}],
       [{call, {?MODULE, f1, [{a}]}}]),

    tr(fun() -> ?MODULE:f1({a}) end,
       {?MODULE, f1, 1},
       [{['$1'],[{'==', '$1', {{a}}}],[]}],
       [{call, {?MODULE, f1, [{a}]}}]),

    %% Undocumented, currently.
    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$1','$1'],[{is_atom, '$1'}],[{message, 4711},
                                        {message, true}]}],
       [{call, {?MODULE, f2, [a, a]}}]),

    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$1','$1'],[{is_atom, '$1'}],[{message, 4711},
                                        {message, false}]}],
       []),

    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$1','$1'],[{is_atom, '$1'}],[kakalorum]}],
       [{call, {?MODULE, f2, [a, a]}}]),

    %% Verify that 'process_dump' can handle a matchstate on the stack.
    tr(fun() -> fbinmatch(<<0>>, 0) end,
       {?MODULE, f1, 1},
       [{['_'],[],[{message, {process_dump}}]}],
       [fun({trace, _, call, {?MODULE, f1, [0]}, _Bin}) -> true end]),

    % Error cases
    errchk([{['$1','$1'],[{is_atom, '$1'}],[{banka, kanin}]}]),
    ok.

test_2(Config) when is_list(Config) ->
    tr(fun() -> ?MODULE:f2(a, a) end,
       {?MODULE, f2, 2},
       [{['$1','$1'],[{is_atom, '$1'}],[{return_trace}]}],
       [{call, {?MODULE, f2, [a, a]}},
        {return_from, {?MODULE, f2, 2}, {a, a}}]),
    ok.

%% Test the enable_trace/2 and caller/0 PAM instructions
test_3(Config) when is_list(Config) ->
    Fun1 = fun() -> 
		   register(fnoppelklopfer,self()),
		   ?MODULE:f2(a, b),
		   ?MODULE:f2(a, b) 
	   end,
    P1 = spawn(?MODULE, runner, [self(), Fun1]),
    Pat = [{['$1','$1'],[],[{message,
                             [{enable_trace, P1, call},{caller}]}]},
           {['_','_'],[],[{message,
                           [{disable_trace, fnoppelklopfer, call}]}]}],
    Fun2 = fun() -> ?MODULE:f3(a, a) end,
    P2 = spawn(?MODULE, runner, [self(), Fun2]),
    erlang:trace(P2, true, [call]),
    erlang:trace_pattern({?MODULE, f2, 2}, Pat),
    collect(P2, [{trace, P2, call, {?MODULE, f2, [a, a]}, [true,
							     {?MODULE,f3,2}]}]),
    collect(P1, [{trace, P1, call, {?MODULE, f2, [a, b]}, [true]}]),
    ok.

%% Test that caller and return to work as they should
%% There was a bug where caller would be undefined when return_to was set
%% for r the bif erlang:put().
caller_and_return_to(Config) ->
    tr(
      fun do_put_wrapper/0,
      fun (Tracee) ->
              MsgCaller = [{'_',[],[{message,{caller}}]}],
              1 = erlang:trace(Tracee, true, [call,return_to]),
              1 = erlang:trace_pattern( {?MODULE,do_put,1}, MsgCaller, [local]),
              1 = erlang:trace_pattern( {?MODULE,do_the_put,1}, MsgCaller, [local]),
              1 = erlang:trace_pattern( {erlang,integer_to_list,1}, MsgCaller, [local]),
              1 = erlang:trace_pattern( {erlang,put,2}, MsgCaller, [local]),

              [{trace,Tracee,call,{?MODULE,do_put,[test]},{?MODULE,do_put_wrapper,0}},
               {trace,Tracee,call,{?MODULE,do_the_put,[test]},{?MODULE,do_put,1}},
               {trace,Tracee,call,{erlang,integer_to_list,[1]},{?MODULE,do_the_put,1}},
               {trace,Tracee,return_to,{?MODULE,do_the_put,1}},
               {trace,Tracee,call,{erlang,put,[test,"1"]},{?MODULE,do_put,1}},
               {trace,Tracee,return_to,{?MODULE,do_put,1}},

               %% These last trace messages are a bit strange...
               %% if call tracing had been enabled for do_put_wrapper
               %% then caller and return_to would have been {?MODULE,do_put_wrapper,1}
               %% but since it is not, they are set to do_put instead, but we still
               %% get the do_put_wrapper return_to message...
               {trace,Tracee,call,{erlang,integer_to_list,[2]},{?MODULE,do_put,1}},
               {trace,Tracee,return_to,{?MODULE,do_put,1}},
               {trace,Tracee,return_to,{?MODULE,do_put_wrapper,0}}
              ]
      end),
    ok.

do_put_wrapper() ->
    do_put(test),
    ok.

do_put(Var) ->
    do_the_put(Var),
    erlang:integer_to_list(id(2)).
do_the_put(Var) ->
    Lst = erlang:integer_to_list(id(1)),
    erlang:put(Var, Lst).

otp_9422(Config) when is_list(Config) ->
    Laps = 10000,
    Fun1 = fun() -> otp_9422_tracee() end,
    P1 = spawn_link(?MODULE, loop_runner, [self(), Fun1, Laps]),
    io:format("spawned ~p as tracee\n", [P1]),

    erlang:trace(P1, true, [call, silent]),

    Fun2 = fun() -> otp_9422_trace_changer() end,
    P2 = spawn_link(?MODULE, loop_runner, [self(), Fun2, Laps]),
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
    erlang:trace_pattern({?MODULE, f1, 1}, Pat1),
    Pat2 = [{[b], [], [{disable_trace, arity}]}],
    erlang:trace_pattern({?MODULE, f1, 1}, Pat2).

    
    


bad_match_spec_bin(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch ets:match_spec_run([1], <<>>)),
    B0 = <<1,2>>,
    {B,_} = split_binary(B0, 0),
    {'EXIT',{badarg,_}} = (catch ets:match_spec_run([1], B)),
    ok.



%% Test the erlang:system_info(trace_control_word) and
%% erlang:system_flag(trace_control_word, Value) BIFs,
%% as well as the get_tcw/0 and set_tcw/1 PAM instructions
trace_control_word(Config) when is_list(Config) ->
    32 = Bits = tcw_bits(),
    High = 1 bsl (Bits - 1),
    erlang:system_flag(trace_control_word, 17),
    tr(fun() -> ?MODULE:f1(a) end,
       {?MODULE, f1, 1},
       [{'_',[{'=:=', {get_tcw}, 17}],[]}],
       [{call, {?MODULE, f1, [a]}}]),
    tr(fun() -> ?MODULE:f1(a) end,
       {?MODULE, f1, 1},
       [{'_',[{'=:=', {get_tcw}, 18}],[]}],
       []),
    erlang:system_flag(trace_control_word, High),
    tr(fun() -> ?MODULE:f1(a) end,
       {?MODULE, f1, 1},
       [{'_',[{'=:=', {get_tcw}, High}],[]}],
       [{call, {?MODULE, f1, [a]}}]),
    erlang:system_flag(trace_control_word, 0),
    tr(fun() -> 
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
    0 = erlang:system_info(trace_control_word),
    ok.

tcw_bits() ->
    tcw_bits(erlang:system_flag(trace_control_word, 0), 0, 0).

tcw_bits(Save, Prev, Bits) ->
    Curr = 1 bsl Bits,
    case catch erlang:system_flag(trace_control_word, Curr) of
        {'EXIT' , {badarg, _}} ->
            Prev = erlang:system_flag(trace_control_word, Save),
            Bits;
        Prev ->
            Curr = erlang:system_info(trace_control_word),
            tcw_bits(Save, Curr, Bits+1)
    end.


%% Test the erlang:trace(_, _, [silent]) flag
%% as well as the silent/0 PAM instruction
silent(Config) when is_list(Config) ->
    %% Global call trace
    tr(fun() -> 
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
    tr(fun() -> 
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

%% Test the erlang:trace(_, _, [silent]) flag without match specs
silent_no_ms(Config) when is_list(Config) ->
    %% Global call trace
    %%
    %% Trace f2/2 and erlang:integer_to_list/1 without match spec
    %% and use match spec on f1/1 to control silent flag.
    tr(
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
              1 = erlang:trace(Tracee, true, [call,silent,return_to]),
              1 = erlang:trace_pattern( {?MODULE,f2,2}, [], [global]),
              1 = erlang:trace_pattern( {erlang,integer_to_list,1}, [], [global]),
              1 = erlang:trace_pattern(
                    {?MODULE,f1,1},
                    [{[start],[],[{silent,false}]},
                     {[stop],[],[{silent,true}]}],
                    [global]),
              %%
              %% Expected: (no return_to for global call trace)
              %%
              [{trace,Tracee,call,{?MODULE,f1,[start]}},
               {trace,Tracee,call,{?MODULE,f2,[f,g]}},
               {trace,Tracee,call,{erlang,integer_to_list,[2]}},
               {trace,Tracee,call,{?MODULE,f2,[h,i]}}]
      end),
    %% Local call trace
    %%
    %% Trace f2/2 and erlang:integer_to_list/1 without match spec
    %% and use match spec on f1/1 to control silent flag.
    tr(
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
              1 = erlang:trace(Tracee, true, [call,silent,return_to]),
              1 = erlang:trace_pattern( {?MODULE,f2,2}, [], [local]),
              1 = erlang:trace_pattern( {erlang,integer_to_list,1}, [], [local]),
              1 = erlang:trace_pattern(
                    {?MODULE,f1,1},
                    [{[start],[],[{silent,false}]},
                     {[stop],[],[{silent,true}]}],
                    [local]),
              %%
              %% Expected:
              %%
              [{trace,Tracee,call,{?MODULE,f1,[start]}},
               {trace,Tracee,return_to,
                {?MODULE,'-silent_no_ms/1-fun-3-',0}},
               {trace,Tracee,call,{?MODULE,f2,[f,g]}},
               {trace,Tracee,return_to,
                {?MODULE,'-silent_no_ms/1-fun-3-',0}},
               {trace,Tracee,call,{erlang,integer_to_list,[2]}},
               {trace,Tracee,return_to,
                {?MODULE,'-silent_no_ms/1-fun-3-',0}},
               {trace,Tracee,call,{?MODULE,f2,[h,i]}},
               {trace,Tracee,return_to,{?MODULE,f3,2}}]
      end).

%% Test that match_spec_test does not activate silent
silent_test(_Config) ->
    {flags,[]} = erlang:trace_info(self(),flags),
    erlang:match_spec_test([],[{'_',[],[{silent,true}]}],trace),
    {flags,[]} = erlang:trace_info(self(),flags).


%% Test the match spec functions {trace/2}
ms_trace2(Config) when is_list(Config) ->
    Tracer = self(),
    %% Meta trace init
    %%
    %% Trace global f1/1, local f2/2, global erlang:integer_to_list/1
    %% without match spec. Use match spec functions
    %% {trace/2} to control trace through fn/2,3.
    tr(
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
              1 = erlang:trace(Tracee, false, [all]),
              1 = erlang:trace_pattern( {?MODULE,f1,1}, [], [global]),
              1 = erlang:trace_pattern( {?MODULE,f2,2}, [], [local]),
              1 = erlang:trace_pattern( {erlang,integer_to_list,1}, [], [global]),
              3 = erlang:trace_pattern(
                    {?MODULE,fn,'_'},
                    [{['$1','$2'],[],
                      [{trace,'$1','$2'},{message,ms_trace2}]}],
                    [meta]),
              %%
              %% Expected: (no return_to for global call trace)
              %%
              Origin = {match_spec_SUITE,'-ms_trace2/1-fun-1-',1},
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
    %% Silence valgrind
    erlang:trace_pattern({?MODULE,fn,'_'},[],[]),
    ok.



%% Test the match spec functions {trace/3}
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
    tr(
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
              1 = erlang:trace(Tracee, false, [all]),
              1 = erlang:trace_pattern( {?MODULE,f1,1}, [], [global]),
              1 = erlang:trace_pattern( {?MODULE,f2,2}, [], [local]),
              1 = erlang:trace_pattern( {erlang,integer_to_list,1}, [], [global]),
              3 = erlang:trace_pattern(
                    {?MODULE,fn,'_'},
                    [{['$1','$2','$3'],[],
                      [{trace,'$1','$2','$3'},{message,Tag}]}],
                    [meta]),
              %%
              %% Expected: (no return_to for global call trace)
              %%
              Origin = {match_spec_SUITE,'-ms_trace3/1-fun-2-',2},
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

%% Test that a dead tracer is removed using ms
ms_trace_dead(_Config) ->
    Self = self(),
    TFun = fun F() -> receive M -> Self ! M, F() end end,
    {Tracer, MRef} = spawn_monitor(TFun),
    MetaTracer = spawn_link(TFun),
    erlang:trace_pattern({?MODULE, f1, '_'},
                         [{'_',[],[{message, false},
                                   {trace,[],
                                    [call,{const,{tracer,Tracer}}]}]}],
                         [{meta, MetaTracer}]),
    erlang:trace_pattern({?MODULE, f2, '_'}, []),
    ?MODULE:f2(1,2),
    ?MODULE:f1(1),
    {tracer,Tracer} = erlang:trace_info(self(), tracer),
    {flags,[call]} = erlang:trace_info(self(), flags),
    ?MODULE:f2(2,3),
    receive {trace, Self, call, {?MODULE, f2, _}} -> ok end,
    exit(Tracer, stop),
    receive {'DOWN',MRef,_,_,_} -> ok end,
    ?MODULE:f1(2),
    {tracer,[]} = erlang:trace_info(self(), tracer),
    ?MODULE:f2(3,4),
    TRef = erlang:trace_delivered(all),
    receive {trace_delivered, _, TRef} -> ok end,
    receive M -> ct:fail({unexpected, M}) after 10 -> ok end.

%% Test that destructive operations in test bif does not really happen
destructive_in_test_bif(Config) when is_list(Config) ->
    {ok,OldToken,_,_} = erlang:match_spec_test
				([],
				 [{'_',[],[{message,{get_seq_token}}]}],trace),
    {ok,_,_,_} = erlang:match_spec_test
			 ([],
			  [{'_',[],[{message,{set_seq_token, label, 1}}]}],
			  trace),
    {ok,OldToken,_,_} = erlang:match_spec_test
				([],
				 [{'_',[],[{message,{get_seq_token}}]}],trace),
    {ok, OldTCW,_,_} = erlang:match_spec_test
			       ([],[{'_',[],[{message,{get_tcw}}]}],trace),
    {ok,OldTCW,_,_} = erlang:match_spec_test
			      ([],
			       [{'_',[],[{message,{set_tcw, OldTCW+1}}]}],
			       trace),
    {ok, OldTCW,_,_} = erlang:match_spec_test
			       ([],[{'_',[],[{message,{get_tcw}}]}],trace),
    ok.

%% Test that the comparison between boxed and small does not crash emulator
boxed_and_small(Config) when is_list(Config) ->
    {ok, Node} = start_node(match_spec_suite_other),
    ok = rpc:call(Node,?MODULE,do_boxed_and_small,[]),
    stop_node(Node),
    ok.

do_boxed_and_small() ->
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{1.47,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{12345678901234567890,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{<<1,2,3,4>>,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{make_ref(),'_'},[],['$_']}],table),
    ok.

%% Test that faulty seq_trace_call does not crash emulator
faulty_seq_trace(Config) when is_list(Config) ->
    {ok, Node} = start_node(match_spec_suite_other),
    ok = rpc:call(Node,?MODULE,do_faulty_seq_trace,[]),
    stop_node(Node),
    ok.

do_faulty_seq_trace() ->
    {ok,'EXIT',_,_} = erlang:match_spec_test([],[{'_',[],[{message,{set_seq_token,yxa,true}}]}],trace),
    ok.

errchk(Pat) ->
    case catch erlang:trace_pattern({?MODULE, f2, 2}, Pat) of
	{'EXIT', {badarg, _}} ->
	    ok;
	Other ->
	    ct:fail({noerror, Other})
    end.

%% Checks that unary minus works
unary_minus(Config) when is_list(Config) ->
    {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'-','$1'},-4}],
				 [true]}],
			       table),
    {ok,false,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'-','$1'},-6}],
				 [true]}],
			       table),
    {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'=:=',{'-','$1',2},3}],
				 [true]}],
			       table),
    {ok,false,[],[]} = erlang:match_spec_test
			      (hej,
			       [{'$1',
				 [{'=/=',{'-','$1'},0}],
				 [true]}],
			       table),
    ok.

%% Checks that unary plus works
unary_plus(Config) when is_list(Config) ->
    {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'+','$1'},6}],
				 [true]}],
			       table),
    {ok,false,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'<',{'+','$1'},4}],
				 [true]}],
			       table),
    {ok,true,[],[]} = erlang:match_spec_test
			      (5,
			       [{'$1',
				 [{'=:=',{'+','$1',2},7}],
				 [true]}],
			       table),
    {ok,false,[],[]} = erlang:match_spec_test
			      (hej,
			       [{'$1',
				 [{'=/=',{'+','$1'},0}],
				 [true]}],
			       table),
    ok.


    

%% Checks that exceptions in guards are handled correctly
guard_exceptions(Config) when is_list(Config) ->
    {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},{'or','$1','$1'}}],
				  [true]}],
				table),
    {ok,true,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'orelse',{is_integer,'$1'},
				    {'or','$1','$1'}}],
				  [true]}],
				table),
    {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'orelse',{'or','$1',true},
				    {is_integer,'$1'}}],
				  [true]}],
				table),
    {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'orelse','$1',true}}],
				  [true]}],
				table),
    {ok,true,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'orelse',true,'$1'}}],
				  [true]}],
				table),
    {ok,true,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'andalso',false,'$1'}}],
				  [true]}],
				table),
    {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'andalso','$1',false}}],
				  [true]}],
				table),

    {ok,false,[],[]} = erlang:match_spec_test
			       (5,
				[{'$1',
				  [{'or',{is_integer,'$1'},
				    {'andalso','$1',false}}],
				  [true]}],
				table),

    ok.

%% Checks floating point exceptions in match-specs
fpe(Config) when is_list(Config) ->
    MS = [{{'$1'},[],[{'/','$1',0}]}],
    case catch (['EXIT','EXIT'] = 
		ets:match_spec_run([{1},{2}],ets:match_spec_compile(MS))) of 
	{'EXIT',_} -> ct:fail({error, "Floating point exceptions faulty"});
	_ -> ok 
    end.

%% Test maps in match-specs
maps(Config) when is_list(Config) ->
    {ok,#{},[],[]} = erlang:match_spec_test(#{}, [{'_',[],['$_']}], table),
    {ok,#{},[],[]} = erlang:match_spec_test(#{}, [{#{},[],['$_']}], table),
    {ok,false,[],[]} =
        erlang:match_spec_test(#{}, [{not_a_map,[],['$_']}], table),
    {ok,bar,[],[]} =
        erlang:match_spec_test(#{foo => bar},
                               [{#{foo => '$1'},[],['$1']}],
                               table),
    {ok,false,[],[]} =
        erlang:match_spec_test(#{foo => bar},
                               [{#{foo => qux},[],[qux]}],
                               table),
    {ok,false,[],[]} =
        erlang:match_spec_test(#{}, [{#{foo => '_'},[],[foo]}], table),
    {error,_} =
        erlang:match_spec_test(#{}, [{#{'$1' => '_'},[],[foo]}], table),
    {ok,bar,[],[]} =
        erlang:match_spec_test({#{foo => bar}},
                               [{{#{foo => '$1'}},[],['$1']}],
                               table),
    {ok,#{foo := 3},[],[]} =
        erlang:match_spec_test({}, [{{},[],[#{foo => {'+',1,2}}]}], table),

    {ok,"camembert",[],[]} =
        erlang:match_spec_test(#{b => "camembert",c => "cabécou"},
                               [{#{b => '$1',c => "cabécou"},[],['$1']}], table),

    {ok,#{a :="camembert",b := "hi"},[],[]} =
        erlang:match_spec_test(#{<<"b">> =>"camembert","c"=>"cabécou", "wat"=>"hi", b=><<"other">>},
                               [{#{<<"b">> => '$1',"wat" => '$2'},[],[#{a=>'$1',b=>'$2'}]}],
                               table),
    %% large maps

    Ls0 = [{I,<<I:32>>}||I <- lists:seq(1,415)],
    M0  = maps:from_list(Ls0),
    M1  = #{a=>1,b=>2,c=>3,d=>4},

    R1  = M0#{263 := #{ a=> 3 }},
    Ms1 = [{M1#{c:='$1'},[],[M0#{263 := #{a => '$1'}}]}],

    {ok,R1,[],[]} = erlang:match_spec_test(M1,Ms1,table),

    Ms2 = [{M0#{63:='$1', 19:='$2'},[],[M0#{19:='$1', 63:='$2'}]}],
    R2  = M0#{63 := maps:get(19,M0), 19 := maps:get(63,M0) },
    {ok,R2,[],[]} = erlang:match_spec_test(M0,Ms2,table),

    ok = maps_check_loop(M1),
    ok = maps_check_loop(M0),
    M2 = maps:from_list([{integer_to_list(K),V} || {K,V} <- Ls0]),
    ok = maps_check_loop(M2),
    ok.

maps_check_loop(M) ->
    Ks = maps:keys(M),
    maps_check_loop(M,M,M,M,Ks,lists:reverse(Ks),1).

maps_check_loop(Orig,M0,MsM0,Rm0,[K|Ks],[Rk|Rks],Ix) ->
    MsK  = list_to_atom([$$]++integer_to_list(Ix)),
    MsM1 = MsM0#{K := MsK},
    Rm1  = Rm0#{Rk := MsK},
    M1   = M0#{Rk  := maps:get(K,MsM0)},
    Ms   = [{MsM1,[],[Rm1]}],
    {ok,M1,[],[]} = erlang:match_spec_test(Orig,Ms,table),
    maps_check_loop(Orig,M1,MsM1,Rm1,Ks,Rks,Ix+1);
maps_check_loop(_,_,_,_,[],[],_) -> ok.


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
    {ok,{ok,false,[1,2,3]},[],[]} =
	erlang:match_spec_test({true,false}, Ms, table),

    Ms2 = [{{'$1','$2'},[],[{{ok,{'orelse','$1','$2'},[1,2,3]}}]}],
    {ok,{ok,true,[1,2,3]},[],[]} =
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
                 fun(X) when is_function(X,1) -> X;
                    (X) -> list_to_tuple([trace, P | tuple_to_list(X)])
                 end,
                 Expected0)
       end).

tr(RunFun, ControlFun) ->
    P = spawn_link(?MODULE, runner, [self(), RunFun]),
    collect(P, ControlFun(P)).

collect(P, TMs) ->
    start_collect(P),
    collect(TMs),
    stop_collect(P).

collect([]) ->
    receive
	M ->
	    io:format("Got unexpected: ~p~n", [M]),
	    flush({got_unexpected,M})
    after 17 ->
	    ok
    end;
collect([TM | TMs]) ->
    io:format(        "Expecting:      ~p~n", [TM]),
    receive
        %% We only look at trace messages with the same tracee
        %% as the message we are looking for. This because
        %% the order of trace messages is only guaranteed from
        %% within a single process.
	M0 when element(2, M0) =:= element(2, TM); is_function(TM, 1) ->
	    M = case element(1, M0) of
		    trace_ts ->
			list_to_tuple(lists:reverse(
					tl(lists:reverse(tuple_to_list(M0)))));
		    _ -> M0
		end,
	    case is_function(TM,1) of
		true ->
		    case (catch TM(M)) of
			true ->
			    io:format("Got:            ~p~n", [M]),
			    collect(TMs);
			_ ->
			    io:format("Got unexpected: ~p~n", [M]),
			    flush({got_unexpected,M})
		    end;

		false ->
		    case M of
			TM ->
			    io:format("Got:            ~p~n", [M]),
			    collect(TMs);
			_ ->
			    io:format("Got unexpected: ~p~n", [M]),
			    flush({got_unexpected,M})
		    end
	    end
    after 15000 ->
            flush(timeout)
    end.

flush(Reason) ->
    receive
        M ->
            io:format("In queue:       ~p~n", [M]),
            flush(Reason)
    after 17 ->
              ct:fail(Reason)
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

fbinmatch(<<Int, Rest/binary>>, Acc) ->
    fbinmatch(Rest, [?MODULE:f1(Int) | Acc]);
fbinmatch(<<>>, Acc) -> Acc.

id(X) ->
    X.

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = atom_to_list(erlang:get_cookie()),
    test_server:start_node(Name, slave, 
                           [{args, "-setcookie " ++ Cookie ++" -pa " ++ Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).
