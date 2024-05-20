%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

-export([all/0, suite/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_1/1, test_2/1, test_3/1, test_4a/1, test_4b/1, test_5a/1,
         test_5b/1, test_6/1, caller_and_return_to/1, bad_match_spec_bin/1,
	 trace_control_word/1, silent/1, silent_no_ms/1, silent_test/1,
	 ms_trace2/1, ms_trace3/1, ms_trace_dead/1, boxed_and_small/1,
	 destructive_in_test_bif/1, guard_exceptions/1,
	 empty_list/1,
	 unary_plus/1, unary_minus/1, moving_labels/1,
         guard_bifs/1]).
-export([fpe/1]).
-export([otp_9422/1]).
-export([faulty_seq_trace/1, do_faulty_seq_trace/0]).
-export([maps/1]).
-export([runner/2, loop_runner/3, fixed_runner/2]).
-export([f1/1, f2/2, f3/2, fn/1, fn/2, fn/3]).
-export([do_boxed_and_small/0]).
-export([f1_test4/1, f2_test4/2, f3_test4/2]).

% This test suite assumes that tracing in general works. What we test is
% the match spec functionality.

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    trace_sessions:all() ++
    testcases_ets() ++
    testcases_match_spec_test().

groups() ->
    trace_sessions:groups(testcases_trace()).

testcases_trace() ->
    [test_1, test_2, test_3, test_4a, test_4b, test_5a, test_5b, test_6,
     caller_and_return_to,
     trace_control_word,
     silent, silent_no_ms, silent_test,
     ms_trace2, ms_trace3, ms_trace_dead,
     otp_9422].

testcases_ets() ->
    [bad_match_spec_bin, fpe].

testcases_match_spec_test() ->
    [boxed_and_small, destructive_in_test_bif,
     guard_exceptions, unary_plus, unary_minus,
     moving_labels, faulty_seq_trace, empty_list,
     maps, guard_bifs].

init_per_suite(Config) ->
    trace_sessions:init_per_suite(Config).

end_per_suite(Config) ->
    trace_sessions:end_per_suite(Config).

init_per_group(Group, Config) ->
    trace_sessions:init_per_group(Group, Config).

end_per_group(Group, Config) ->
    trace_sessions:end_per_group(Group, Config).

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    (erts_test_utils:ept_check_leaked_nodes(Config)
     andalso
     trace_sessions:end_per_testcase(Config)).

erlang_trace(A,B,C) ->
    trace_sessions:erlang_trace(A,B,C).

erlang_trace_pattern(A,B) ->
    trace_sessions:erlang_trace_pattern(A,B).

erlang_trace_pattern(A,B,C) ->
    %%erlang:display({?LINE, "B", B}),
    trace_sessions:erlang_trace_pattern(A,B,C).

erlang_trace_info(A,B) ->
    trace_sessions:erlang_trace_info(A,B).


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
    erlang_trace(P2, true, [call]),
    erlang_trace_pattern({?MODULE, f2, 2}, Pat),
    collect(P2, [{trace, P2, call, {?MODULE, f2, [a, a]}, [true,
							     {?MODULE,f3,2}]}]),
    collect(P1, [{trace, P1, call, {?MODULE, f2, [a, b]}, [true]}]),
    ok.

%% Test `caller_line` trace with `call` and `global`
test_4a(Config) when is_list(Config) ->
    Fun = fun() -> ?MODULE:f3_test4(a, b) end,
    Pat = [{'_', [],[{message, {caller_line}}]}],
    P = spawn(?MODULE, runner, [self(), Fun]),
    erlang_trace(P, true, [call]),
    %% `global` is implied but we still mention explicitly
    erlang_trace_pattern({?MODULE, f2_test4, 2}, Pat, [global]),
    erlang_trace_pattern({?MODULE, f1_test4, 1}, Pat, [global]),
    collect(P, [{trace, P, call, {?MODULE, f2_test4, [a, b]}, {?MODULE, f3_test4, 2, {"test4.erl", 3}}},
                {trace, P, call, {?MODULE, f1_test4, [a]}, {?MODULE, f3_test4, 2, {"test4.erl", 3}}}]),
    ok.

%% Test `caller_line` trace with `return_trace`, `call` and `global`
test_4b(Config) when is_list(Config) ->
    Fun = fun() -> ?MODULE:f3_test4(a, b) end,
    P = spawn(?MODULE, runner, [self(), Fun]),
    Pat = [{'_', [], [{return_trace}, {message, {caller_line}}]}],
    erlang_trace(P, true, [call]),
    %% `global` is implied but we still mention explicitly
    erlang_trace_pattern({?MODULE, f2_test4, 2}, Pat, [global]),
    erlang_trace_pattern({?MODULE, f1_test4, 1}, Pat, [global]),
    collect(P, [{trace, P, call, {?MODULE, f2_test4, [a, b]}, {?MODULE, f3_test4, 2, {"test4.erl", 3}}},
                {trace, P, call, {?MODULE, f1_test4, [a]}, {?MODULE, f3_test4, 2, {"test4.erl", 3}}},
                {trace, P, return_from, {?MODULE, f1_test4, 1}, {a}},
                {trace, P, return_from, {?MODULE, f2_test4, 2}, {a}}
               ]),
    ok.

%% Test `caller_line` trace with `call` and `local`
test_5a(Config) when is_list(Config) ->
    Fun = fun() -> f3_test5(a, b) end,
    Pat = [{'_', [],[{message, {caller_line}}]}],
    P = spawn(?MODULE, runner, [self(), Fun]),
    erlang_trace(P, true, [call]),
    %% Notice `local` function tracing
    erlang_trace_pattern({?MODULE, f2_test5, 2}, Pat, [local]),
    erlang_trace_pattern({?MODULE, f1_test5, 1}, Pat, [local]),
    collect(P, [{trace, P, call, {?MODULE, f2_test5, [a, b]}, {?MODULE, f3_test5, 2, {"test5.erl", 3}}},
                {trace, P, call, {?MODULE, f1_test5, [a]}, {?MODULE, f3_test5, 2, {"test5.erl", 3}}}]),
    ok.

%% Test `caller_line` trace with `return_trace`, `call` and `local`
test_5b(Config) when is_list(Config) ->
    Fun = fun() -> f3_test5(a, b) end,
    P = spawn(?MODULE, runner, [self(), Fun]),
    Pat = [{'_', [], [{return_trace}, {message, {caller_line}}]}],
    erlang_trace(P, true, [call]),
    %% Notice `local` function tracing
    erlang_trace_pattern({?MODULE, f2_test5, 2}, Pat, [local]),
    erlang_trace_pattern({?MODULE, f1_test5, 1}, Pat, [local]),
    collect(P, [{trace, P, call, {?MODULE, f2_test5, [a, b]}, {?MODULE, f3_test5, 2, {"test5.erl", 3}}},
                {trace, P, call, {?MODULE, f1_test5, [a]}, {?MODULE, f3_test5, 2, {"test5.erl", 3}}},
                {trace, P, return_from, {?MODULE, f1_test5, 1}, {a}},
                {trace, P, return_from, {?MODULE, f2_test5, 2}, {a}}
               ]),
    ok.

%% Test current_stacktrace/[0,1]
test_6(Config) when is_list(Config) ->
    %% Test non small argument
    case catch erlang_trace_pattern({?MODULE, f2_test6, '_'},
                                    [{'_', [], [{message, {current_stacktrace, a}}]}]) of
        {'EXIT', {badarg, _}} -> ok;
        Other1 -> ct:fail({noerror, Other1})
    end,

    %% Test negative
    case catch erlang_trace_pattern({?MODULE, f2_test6, '_'},
                                    [{'_', [], [{message, {current_stacktrace, -1}}]}]) of
        {'EXIT', {badarg, _}} -> ok;
        Other2 -> ct:fail({noerror, Other2})
    end,

    Fun = fun() -> f5_test6() end,
    Pat = [{'_', [], [{message, {current_stacktrace}}]}],
    P = spawn(?MODULE, fixed_runner, [self(), Fun]),
    erlang_trace(P, true, [call]),
    erlang_trace_pattern({?MODULE, f2_test6, 1}, Pat, [local]),
    erlang_trace_pattern({?MODULE, f1_test6, 0}, Pat, [local]),
    collect(P, [{trace, P, call, {?MODULE, f2_test6, [f1]},
                   [
                    {?MODULE, f3_test6, 0, [{file, "test6.erl"}, {line, 21}]},
                    {?MODULE, f5_test6, 0, [{file, "test6.erl"}, {line, 14}]},
                    {?MODULE, fixed_runner, 2, [{file, "test6.erl"}, {line, 7}]}
                   ]},
                {trace, P, call, {?MODULE, f1_test6, []},
                   [
                    {?MODULE, f2_test6, 1, [{file, "test6.erl"}, {line, 25}]},
                    {?MODULE, f3_test6, 0, [{file, "test6.erl"}, {line, 21}]},
                    {?MODULE, f5_test6, 0, [{file, "test6.erl"}, {line, 14}]},
                    {?MODULE, fixed_runner, 2, [{file, "test6.erl"}, {line, 7}]}
                   ]}
               ]),

    Pat2 = [{'_', [], [{message, {current_stacktrace, 3}}]}],
    P2 = spawn(?MODULE, fixed_runner, [self(), Fun]),
    erlang_trace(P2, true, [call]),
    erlang_trace_pattern({?MODULE, f2_test6, 1}, Pat2, [local]),
    erlang_trace_pattern({?MODULE, f1_test6, 0}, Pat2, [local]),
    collect(P2, [{trace, P2, call, {?MODULE, f2_test6, [f1]},
                   [
                    {?MODULE, f3_test6, 0, [{file, "test6.erl"}, {line, 21}]},
                    {?MODULE, f5_test6, 0, [{file, "test6.erl"}, {line, 14}]},
                    {?MODULE, fixed_runner, 2, [{file, "test6.erl"}, {line, 7}]}
                   ]},
                {trace, P2, call, {?MODULE, f1_test6, []},
                   [
                    {?MODULE, f2_test6, 1, [{file, "test6.erl"}, {line, 25}]},
                    {?MODULE, f3_test6, 0, [{file, "test6.erl"}, {line, 21}]},
                    {?MODULE, f5_test6, 0, [{file, "test6.erl"}, {line, 14}]}
                   ]}
               ]),

    %% Test when erts_backtrace_depth is less than depth
    OldDepth = erlang:system_flag(backtrace_depth, 2),
    try
        P3 = spawn(?MODULE, fixed_runner, [self(), Fun]),
        erlang_trace(P3, true, [call]),
        erlang_trace_pattern({?MODULE, f2_test6, 1}, Pat2, [local]),
        erlang_trace_pattern({?MODULE, f1_test6, 0}, Pat2, [local]),
        collect(P3, [{trace, P3, call, {?MODULE, f2_test6, [f1]},
                       [
                        {?MODULE, f3_test6, 0, [{file, "test6.erl"}, {line, 21}]},
                        {?MODULE, f5_test6, 0, [{file, "test6.erl"}, {line, 14}]}
                       ]},
                    {trace, P3, call, {?MODULE, f1_test6, []},
                       [
                        {?MODULE, f2_test6, 1, [{file, "test6.erl"}, {line, 25}]},
                        {?MODULE, f3_test6, 0, [{file, "test6.erl"}, {line, 21}]}
                       ]}
                   ])
    after
        erlang:system_flag(backtrace_depth, OldDepth)
    end,

    ok.

%% Test that caller and return to work as they should
%% There was a bug where caller would be undefined when return_to was set
%% for r the bif erlang:put().
caller_and_return_to(Config) when is_list(Config) ->
    tr(
      fun do_put_wrapper/0,
      fun (Tracee) ->
              MsgCaller = [{'_',[],[{message,{caller}}]}],
              1 = erlang_trace(Tracee, true, [call,return_to]),
              1 = erlang_trace_pattern( {?MODULE,do_put,1}, MsgCaller, [local]),
              1 = erlang_trace_pattern( {?MODULE,do_the_put,1}, MsgCaller, [local]),
              1 = erlang_trace_pattern( {erlang,integer_to_list,1}, MsgCaller, [local]),
              1 = erlang_trace_pattern( {erlang,put,2}, MsgCaller, [local]),

              [{trace,Tracee,call,{?MODULE,do_put,[test]},{?MODULE,do_put_wrapper,0}},
               {trace,Tracee,call,{?MODULE,do_the_put,[test]},{?MODULE,do_put,1}},
               {trace,Tracee,call,{erlang,integer_to_list,[1]},{?MODULE,do_the_put,1}},
               {trace,Tracee,return_to,{?MODULE,do_the_put,1}},
               {trace,Tracee,call,{erlang,put,[test,"1"]},{?MODULE,do_the_put,1}},
               {trace,Tracee,return_to,{?MODULE,do_the_put,1}},
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

    erlang_trace(P1, true, [call, silent]),

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
    erlang_trace_pattern({?MODULE, f1, 1}, Pat1),
    Pat2 = [{[b], [], [{disable_trace, arity}]}],
    erlang_trace_pattern({?MODULE, f1, 1}, Pat2).

    
    


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
              1 = erlang_trace(Tracee, true, [call,silent,return_to]),
              1 = erlang_trace_pattern( {?MODULE,f2,2}, [], [global]),
              1 = erlang_trace_pattern( {erlang,integer_to_list,1}, [], [global]),
              1 = erlang_trace_pattern(
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
              1 = erlang_trace(Tracee, true, [call,silent,return_to]),
              1 = erlang_trace_pattern( {?MODULE,f2,2}, [], [local]),
              1 = erlang_trace_pattern( {erlang,integer_to_list,1}, [], [local]),
              1 = erlang_trace_pattern(
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
    {flags,[]} = erlang_trace_info(self(),flags),
    erlang:match_spec_test([],[{'_',[],[{silent,true}]}],trace),
    {flags,[]} = erlang_trace_info(self(),flags).


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
              1 = erlang_trace(Tracee, false, [all]),
              1 = erlang_trace_pattern( {?MODULE,f1,1}, [], [global]),
              1 = erlang_trace_pattern( {?MODULE,f2,2}, [], [local]),
              1 = erlang_trace_pattern( {erlang,integer_to_list,1}, [], [global]),
              3 = erlang_trace_pattern(
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
    erlang_trace_pattern({?MODULE,fn,'_'},[],[]),
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
              1 = erlang_trace(Tracee, false, [all]),
              1 = erlang_trace_pattern( {?MODULE,f1,1}, [], [global]),
              1 = erlang_trace_pattern( {?MODULE,f2,2}, [], [local]),
              1 = erlang_trace_pattern( {erlang,integer_to_list,1}, [], [global]),
              3 = erlang_trace_pattern(
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
    erlang_trace_pattern({?MODULE, f1, '_'},
                         [{'_',[],[{message, false},
                                   {trace,[],
                                    [call,{const,{tracer,Tracer}}]}]}],
                         [{meta, MetaTracer}]),
    erlang_trace_pattern({?MODULE, f2, '_'}, []),
    ?MODULE:f2(1,2),
    ?MODULE:f1(1),
    {tracer,Tracer} = erlang_trace_info(self(), tracer),
    {flags,[call]} = erlang_trace_info(self(), flags),
    ?MODULE:f2(2,3),
    receive {trace, Self, call, {?MODULE, f2, _}} -> ok end,
    exit(Tracer, stop),
    receive {'DOWN',MRef,_,_,_} -> ok end,
    ?MODULE:f1(2),
    {tracer,[]} = erlang_trace_info(self(), tracer),
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
    {ok, Peer, Node} = ?CT_PEER(),
    ok = rpc:call(Node,?MODULE,do_boxed_and_small,[]),
    peer:stop(Peer).

do_boxed_and_small() ->
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{1.47,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{12345678901234567890,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{<<1,2,3,4>>,'_'},[],['$_']}],table),
    {ok, false, _, _} = erlang:match_spec_test({0,3},[{{make_ref(),'_'},[],['$_']}],table),
    ok.

%% Test that faulty seq_trace_call does not crash emulator
faulty_seq_trace(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    ok = rpc:call(Node,?MODULE,do_faulty_seq_trace,[]),
    peer:stop(Peer).

do_faulty_seq_trace() ->
    {ok,'EXIT',_,_} = erlang:match_spec_test([],[{'_',[],[{message,{set_seq_token,yxa,true}}]}],trace),
    ok.

errchk(Pat) ->
    case catch erlang_trace_pattern({?MODULE, f2, 2}, Pat) of
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

    {ok,1,[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[],[{map_size,'$1'}]}],table),
    {ok,'EXIT',[],[]} = erlang:match_spec_test(not_a_map, [{'$1',[],[{map_size,'$1'}]}], table),
    {ok,false,[],[]} = erlang:match_spec_test(not_a_map, [{'$1',[{map_size,'$1'}],['$_']}], table),
    {ok,true,[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[{'=:=',{map_size,'$1'},1}],[true]}], table),

    {ok,1,[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[],[{map_get,a,'$1'}]}], table),
    {ok,'EXIT',[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[],[{map_get,b,'$1'}]}], table),
    {ok,'EXIT',[],[]} = erlang:match_spec_test(not_a_map, [{'$1',[],[{map_get,b,'$1'}]}], table),
    {ok,false,[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[{map_get,b,'$1'}],['$_']}], table),
    {ok,false,[],[]} = erlang:match_spec_test(not_a_map, [{'$1',[{map_get,b,'$1'}],['$_']}], table),
    {ok,true,[],[]} = erlang:match_spec_test(#{a => true}, [{'$1',[{map_get,a,'$1'}],[true]}], table),

    {ok,true,[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[],[{is_map_key,a,'$1'}]}], table),
    {ok,false,[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[],[{is_map_key,b,'$1'}]}], table),
    {ok,'EXIT',[],[]} = erlang:match_spec_test(not_a_map, [{'$1',[],[{is_map_key,a,'$1'}]}], table),
    {ok,false,[],[]} = erlang:match_spec_test(#{a => 1}, [{'$1',[{is_map_key,b,'$1'}],['$_']}], table),
    {ok,false,[],[]} = erlang:match_spec_test(not_a_map, [{'$1',[{is_map_key,b,'$1'}],['$_']}], table),
    {ok,true,[],[]} = erlang:match_spec_test(#{a => true}, [{'$1',[{is_map_key,a,'$1'}],[true]}], table),

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

    %% Maps in guards
    {ok,#{a:=1},[],[]} = erlang:match_spec_test(#{a=>1}, [{'$1',[{'==','$1',#{a=>1}}],['$1']}], table),
    {ok,#{a:='$1'},[],[]} = erlang:match_spec_test(#{a=>'$1'}, [{'$1',[{'==','$1',#{a=>{const,'$1'}}}],['$1']}], table),
    {ok,#{a:=1},[],[]} = erlang:match_spec_test(#{a=>1}, [{'$1',[{'==','$1',#{{const,a}=>1}}],['$1']}], table),
    {ok,#{20:=1,b:=2},[],[]} = erlang:match_spec_test({11,#{20=>1,b=>2}},[{{'$1','$2'},[{'==','$2',#{{'+','$1',9}=>{'-','$1',10},b=>{const,2}}}],['$2']}], table),
    %% Test that maps with duplicate keys work. This depends on the iteration order of small maps.
    true = lists:any(
             fun(N) ->
                     {ok,#{1=>1,2=>N},[],[]} ==
                         erlang:match_spec_test(#{1=>1,2=>N},[{'$1',[{'==','$1',#{1=>1,2=>2,{const,2}=>3,{'+',1,1}=>4,{'+',2,0}=>5}}],['$1']}],table)
             end,[2,3,4,5]),
    %% Test what happens when a map is collapsed from hash to flatmap
    {ok,#{0:=1},[],[]} = erlang:match_spec_test(#{0=>1},[{'$1',[{'==','$1',maps:from_list([{{'-',I,I},1} || I <- lists:seq(1,100)])}],['$1']}], table),

    %% Large maps in guards
    {ok,#{a:=1},[],[]} = erlang:match_spec_test(M0#{a=>1}, [{'$1',[{'==','$1',M0#{a=>1}}],['$1']}], table),
    {ok,#{a:='$1'},[],[]} = erlang:match_spec_test(M0#{a=>'$1'}, [{'$1',[{'==','$1',M0#{a=>{const,'$1'}}}],['$1']}], table),
    {ok,#{520:=1,b:=2},[],[]} = erlang:match_spec_test({11,M0#{520=>1,b=>2}},[{{'$1','$2'},[{'==','$2',M0#{{'+','$1',509}=>{'-','$1',10},b=>{const,2}}}],['$2']}], table),
    %% Test that maps with duplicate keys work. This depends on the iteration order of hash maps.
    true = lists:any(
             fun(N) ->
                     {ok,M0#{1:=1,2:=N},[],[]} == erlang:match_spec_test(M0#{1=>1,2=>N},[{'$1',[{'==','$1',M0#{1=>1,2=>2,{const,2}=>3,{'+',1,1}=>4,{'+',2,0}=>5}}],['$1']}], table)
             end, [2,3,4,5]),

    %% Maps in body
    {ok,#{a:=1,b:=2},[],[]} = erlang:match_spec_test(#{a=>1},[{#{a=>'$1'},[],[#{a=>'$1',b=>{const,2}}]}], table),
    {ok,#{a:=1,b:=#{a:='$1'}},[],[]} = erlang:match_spec_test(#{a=>1},[{#{a=>'$1'},[],[#{a=>'$1',b=>#{a=>{const,'$1'}}}]}], table),
    {ok,#{a:=1,b:=#{a:='$1'}},[],[]} = erlang:match_spec_test(#{a=>1},[{#{a=>'$1'},[],[#{a=>'$1',{const,b}=>#{a=>{const,'$1'}}}]}], table),
    %% Test that maps with duplicate keys work. This depends on the iteration order of small maps.
    true = lists:any(
             fun(N) ->
                     {ok,#{2=>N},[],[]} == erlang:match_spec_test(#{a=>1},[{#{a=>'$1'},[],[#{{'+',3,-1}=>1,2=>2,{const,2}=>3,{'+',1,'$1'}=>4,{'+',2,0}=>5}]}], table)
             end,[1,2,3,4,5]),
    %% Test what happens when a map is collapsed from hash to flatmap
    {ok,#{0:=1},[],[]} = erlang:match_spec_test(#{0=>1},[{'$1',[],[maps:from_list([{{'-',I,I},1} || I <- lists:seq(1,100)])]}], table),

    %% Large maps in body
    {ok,#{a:=1,b:=#{a:='$1'}},[],[]} = erlang:match_spec_test(M0#{a=>1},[{#{a=>'$1'},[],[M0#{a=>'$1',b=>M0#{a=>{const,'$1'}}}]}], table),
    {ok,#{a:=1,b:=#{a:='$1'}},[],[]} = erlang:match_spec_test(M0#{a=>1},[{#{a=>'$1'},[],[M0#{a=>'$1',{const,b}=>M0#{a=>{const,'$1'}}}]}], table),
    %% Test that maps with duplicate keys work. This depends on the iteration order of hash maps.
    true = lists:any(
             fun(N) ->
                     {ok,M0#{1:=1,2:=N},[],[]} == erlang:match_spec_test(M0#{1=>1,2=>5},[{'$1',[],[M0#{1=>1,2=>2,{const,2}=>3,{'+',1,1}=>4,{'*',2,1}=>5}]}], table)
             end,[2,3,4,5]),

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

-record(dummy_record, {}).

%% GH-7045: Some guard BIFs were unavailable in match specifications.
guard_bifs(_Config) ->
    Matches =
        [begin
             BIF = list_to_tuple([F | lists:duplicate(A, '$1')]),
             erlang:match_spec_test(Data, [{{'$1'}, [BIF], [{{'$1'}}]}], Kind)
         end
         || {F, A} <- erlang:module_info(functions),
            {Data, Kind} <- [{{a}, table}, {[a], trace}],
            F =/= is_record, %% Has special requirements, checked below.
            erl_internal:arith_op(F, A) orelse
                erl_internal:bool_op(F, A) orelse
                erl_internal:comp_op(F, A) orelse
                erl_internal:guard_bif(F, A)],
    [] = [T || {error, _}=T <- Matches],

    IsRecord = {is_record,
                '$1',
                dummy_record,
                record_info(size, dummy_record)},
    [{ok, _, [], []} =
         erlang:match_spec_test(Data, [{{'$1'}, [IsRecord], [{{'$1'}}]}], Kind)
     || {Data, Kind} <- [{{#dummy_record{}}, table},
                         {[#dummy_record{}], trace}]],

    ok.

tr(Fun, MFA, Pat, Expected) ->
    tr(Fun, MFA, [call], Pat, [global], Expected).

tr(Fun, MFA, TraceFlags, Pat, PatFlags, Expected0) ->
    tr(Fun,
       fun(P) ->
               erlang_trace(P, true, TraceFlags),
               erlang_trace_pattern(MFA, Pat, PatFlags),
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
            erlang:display({?LINE,got_unexpected, M}),
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
                        erlang:delete_element(tuple_size(M0), M0);
		    _ -> M0
		end,
	    case is_function(TM,1) of
		true ->
		    case (catch TM(M)) of
			true ->
			    io:format("Got:            ~p~n", [M]),
			    collect(TMs);
			_ ->
                            erlang:display({?LINE,got_unexpected, M}),
			    io:format("Got unexpected: ~p~n", [M]),
			    flush({got_unexpected,M})
		    end;

		false ->
		    case M of
			TM ->
			    io:format("Got:            ~p~n", [M]),
			    collect(TMs);
			_ ->
                            erlang:display({?LINE,got_unexpected, M}),
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

-file("test4.erl", 1).
f3_test4(X,Y) ->
    ?MODULE:f2_test4(X,Y), % Line 3 - This line number should remain stable
    ok.

f2_test4(X, _) ->
    ?MODULE:f1_test4(X).

f1_test4(X) ->
    {X}.

-file("test5.erl", 1).
f3_test5(X,Y) ->
    f2_test5(X,Y), % Line 3 - This line number should remain stable
    ok.

f2_test5(X, _) ->
    f1_test5(X).

f1_test5(X) ->
    {X}.

-file("test6.erl", 1).
fixed_runner(Collector, Fun) ->
    receive
        {go, Collector} ->
            go
    end,
    Fun(), % Line 7 - This line number should remain stable
    receive
        {done, Collector} ->
            Collector ! {gone, self()}
    end.

f5_test6() ->
    f3_test6(), % Line 14 - This line number should remain stable
    f4_test6().

f4_test6() ->
    f4.

f3_test6() ->
    f2_test6(f1), % Line 21 - This line number should remain stable
    f3.

f2_test6(X) ->
    X = f1_test6(), % Line 25 - This line number should remain stable
    f2.

f1_test6() ->
    f1.
