%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(erl_error_SUITE).
-moduledoc false.

%% Tests for erl_error:format_exception/3,4.
%%
%% Besides asserting formatting correctness, every test case also
%% reports the memory used to produce the formatted result, so the
%% impact of switching erl_error from io_lib:format (deep char lists)
%% to a binary-producing variant (io_lib:bformat) can be measured.
%%
%% Two classes of numbers are reported per case (see report_mem/2):
%%
%%   * retained size of the returned chardata
%%       - erts_debug:size/1      heap words incl. sharing
%%       - erts_debug:flat_size/1 heap words without sharing
%%       - iolist_size/1          logical byte length
%%   * transient allocation while formatting, obtained by tracing
%%     garbage_collection on an isolated worker process and summing
%%     the reclaimed heap over a number of iterations.

-include_lib("common_test/include/ct.hrl").

%% ct callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% test cases
-export([error_badarg/1, error_badmatch/1, error_function_clause/1,
         exit_reason/1, throw_reason/1,
         error_info_cause/1, error_info_general_reason/1,
         deep_stacktrace/1, trimmed_stacktrace/1,
         unicode_reason/1, chars_limit/1, column_indent/1,
         big_state_term/1, huge_proplist_state/1, unicode_frames/1]).

%% helper exported so it can be spawned on a clean process
-export([format_worker/2]).

%% Standalone development harness (run outside Common Test).
-export([run/0, run/1]).

-define(ITERATIONS, 100).

%% For the huge-state reproduction (huge_proplist_state/1). The
%% original report used 48322 instances; we use fewer by default to
%% keep the suite fast, and format it fewer times because each format
%% is expensive. Increase to reproduce the original scale.
-define(HUGE_INSTANCES, 5000).
-define(HUGE_ITERATIONS, 5).

%% Number of independent trials to average the transient-allocation
%% measurement over (GC timing makes a single trial noisy).
-define(TRIALS, 5).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() ->
    [{group,correctness}].

groups() ->
    [{correctness,[parallel],
      [error_badarg, error_badmatch, error_function_clause,
       exit_reason, throw_reason,
       error_info_cause, error_info_general_reason,
       deep_stacktrace, trimmed_stacktrace,
       unicode_reason, chars_limit, column_indent,
       big_state_term, huge_proplist_state, unicode_frames]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%%
%%% Standalone development harness
%%%
%%% Run all (or selected) cases without Common Test, e.g.:
%%%
%%%   erl_error_SUITE:run().
%%%   erl_error_SUITE:run([deep_stacktrace, big_state_term]).
%%%
%%% Each case prints its memory profile (see report_mem/2) and the
%%% harness prints a pass/fail summary at the end. Returns ok if all
%%% selected cases pass, {error, Failures} otherwise.

-doc false.
run() ->
    run(cases()).

-doc false.
run(Cases) when is_list(Cases) ->
    Results =
        [begin
             R = try apply(?MODULE, Case, [[]]) of
                     {comment, _} -> ok;
                     Other -> {unexpected, Other}
                 catch
                     Class:Reason:Stk -> {'EXIT', {Class, Reason, Stk}}
                 end,
             {Case, R}
         end || Case <- Cases],
    io:format("~n===== erl_error_SUITE summary =====~n", []),
    lists:foreach(
      fun({Case, ok}) ->
              io:format("  ~-28w PASS~n", [Case]);
         ({Case, Bad}) ->
              io:format("  ~-28w FAIL ~P~n", [Case, Bad, 8])
      end, Results),
    case [C || {C, R} <- Results, R =/= ok] of
        [] ->
            io:format("~nall ~p cases passed~n", [length(Results)]),
            ok;
        Failures ->
            io:format("~n~p FAILURE(s): ~p~n", [length(Failures), Failures]),
            {error, Failures}
    end.

cases() ->
    {correctness, _Opts, Cases} = lists:keyfind(correctness, 1, groups()),
    Cases.

%%%
%%% Test cases
%%%
%%% Each case builds an exception (Class, Reason, Stacktrace), calls
%%% format_exception, asserts the essentials, and then reports memory.

error_badarg(_Config) ->
    {C,R,Stk} = capture(fun() -> erlang:error(badarg) end),
    Str = fmt(C, R, Stk),
    "exception error: bad argument" ++ _ = flat(Str),
    report_mem(error_badarg, {C,R,Stk}).

error_badmatch(_Config) ->
    V = id([1,2,3]),
    {C,R,Stk} = capture(fun() -> [_] = V end),
    Str = fmt(C, R, Stk),
    true = contains(flat(Str), "no match of right hand side value"),
    report_mem(error_badmatch, {C,R,Stk}).

error_function_clause(_Config) ->
    {C,R,Stk} = capture(fun() -> lists:keyfind(a, b, c) end),
    Str = fmt(C, R, Stk),
    true = is_chardata(Str),
    report_mem(error_function_clause, {C,R,Stk}).

exit_reason(_Config) ->
    {C,R,Stk} = capture(fun() -> exit(shutdown) end),
    Str = fmt(C, R, Stk),
    "exception exit: shutdown" ++ _ = flat(Str),
    report_mem(exit_reason, {C,R,Stk}).

throw_reason(_Config) ->
    {C,R,Stk} = capture(fun() -> throw(my_error) end),
    Str = fmt(C, R, Stk),
    "exception throw: my_error" ++ _ = flat(Str),
    report_mem(throw_reason, {C,R,Stk}).

error_info_cause(_Config) ->
    %% EEP-54: a BIF that provides per-argument cause information.
    {C,R,Stk} = capture(fun() -> erlang:atom_to_list(id(42)) end),
    Str = fmt(C, R, Stk),
    F = flat(Str),
    true = contains(F, "exception error"),
    true = is_chardata(Str),
    report_mem(error_info_cause, {C,R,Stk}).

error_info_general_reason(_Config) ->
    %% User supplied error_info with general + reason keys.
    {C,R,Stk} =
        capture(fun() ->
                        erlang:error(my_reason, [1],
                                     [{error_info,
                                       #{module => ?MODULE,
                                         cause => #{1 => "should be an atom"},
                                         general => "extra general info"}}])
                end),
    Str = fmt(C, R, Stk),
    true = is_chardata(Str),
    report_mem(error_info_general_reason, {C,R,Stk}).

deep_stacktrace(_Config) ->
    {C,R,Stk0} = capture(fun() -> erlang:error(deep) end),
    %% Fabricate a long stacktrace to stress stack formatting. Real
    %% stacktraces carry an argument list only on the top frame; all
    %% deeper frames are arity-only (an integer), so the synthetic
    %% frames use integer arity to match real-world cost.
    Extra = [{some_module, some_function, 4, [{file,"m.erl"},{line,N}]}
             || N <- lists:seq(1, 50)],
    Stk = Stk0 ++ Extra,
    Str = fmt(C, R, Stk),
    true = is_chardata(Str),
    report_mem(deep_stacktrace, {C,R,Stk}).

trimmed_stacktrace(_Config) ->
    {C,R,Stk} = capture(fun() -> erlang:error(trim_me) end),
    SF = fun(erl_eval, _, _) -> true; (_, _, _) -> false end,
    Str = erl_error:format_exception(C, R, Stk, #{stack_trim_fun => SF}),
    true = is_chardata(Str),
    report_mem(trimmed_stacktrace, {C,R,Stk},
               #{stack_trim_fun => SF}).

unicode_reason(_Config) ->
    %% A reason carrying non-latin1 code points.
    Reason = {failed, [1089, 1090, 1088, "тест"]},
    {C,R,Stk} = capture(fun() -> erlang:error(Reason) end),
    Str = erl_error:format_exception(C, R, Stk, #{}),
    true = is_chardata(Str),
    report_mem(unicode_reason, {C,R,Stk}).

chars_limit(_Config) ->
    Big = lists:seq(1, 5000),
    {C,R,Stk} = capture(fun() -> erlang:error({too_big, Big}) end),
    %% chars_limit is only reachable via the internal arity-8 export,
    %% which requires an arity-3 FormatFun: fun(Term, Col, CL) -> {IoList, CL}.
    FF = fun(T, I, CL) -> {io_lib:print(T, I, 80, 30), CL} end,
    SF = fun(_, _, _) -> false end,
    Str = erl_error:format_exception(1, C, R, Stk, SF, FF, unicode, 200),
    true = is_chardata(Str),
    %% Rough sanity: limited output is far smaller than unlimited.
    Unlimited = erl_error:format_exception(C, R, Stk, #{}),
    true = iolist_size(Str) < iolist_size(Unlimited),
    report_mem(chars_limit, {C,R,Stk}).

column_indent(_Config) ->
    {C,R,Stk} = capture(fun() -> erlang:error(indented) end),
    Str = erl_error:format_exception(C, R, Stk, #{column => 5}),
    true = is_chardata(Str),
    report_mem(column_indent, {C,R,Stk}, #{column => 5}).

big_state_term(_Config) ->
    %% Emulate a gen_server-style crash carrying a large state term.
    State = #{data => lists:seq(1, 2000),
              nested => [{k, V, <<"payload">>} || V <- lists:seq(1, 200)]},
    {C,R,Stk} = capture(fun() -> erlang:error({bad_state, State}) end),
    Str = fmt(C, R, Stk),
    true = is_chardata(Str),
    report_mem(big_state_term, {C,R,Stk}).

%% Reproduction of a reported problem: a process crash whose exception
%% carries a very large state term (tens of thousands of proplist
%% entries). Formatting such a term with erl_error produced a large
%% amount of memory. Modelled after the original report; all atoms and
%% strings have been replaced with dummy names.
%%
%% The original used 48322 instances; ?HUGE_INSTANCES keeps the suite
%% within a reasonable time/memory budget while remaining
%% representative. Bump it (or call huge_state_term/1 directly) to
%% reproduce the original scale.
huge_proplist_state(_Config) ->
    {C,R,Stk} = huge_crash(?HUGE_INSTANCES),
    Str = fmt(C, R, Stk),
    true = is_chardata(Str),

    %% Boundedness: erl_error's default format fun is depth-limited
    %% (io_lib:print(Term, I, 80, 30)), so the formatted output must
    %% not grow proportionally to the number of instances. Compare a
    %% small state against the large one; the sizes should be within a
    %% small constant factor, not the ~10x ratio of the instance
    %% counts. This documents (and guards) that a huge state does not
    %% blow up the formatted error report.
    {Cs,Rs,Ss} = huge_crash(?HUGE_INSTANCES div 10),
    SmallBytes = iolist_size(fmt(Cs, Rs, Ss)),
    BigBytes = iolist_size(Str),
    true = BigBytes =< SmallBytes * 2,

    %% Fewer iterations than the default: the term is huge and each
    %% format allocates a lot.
    report_mem(huge_proplist_state, {C,R,Stk}, #{}, ?HUGE_ITERATIONS).

%% Stacktrace whose frames carry non-latin1 module/function/file names.
%% Exercises correct unicode (~ts) handling of accumulated frame text
%% when the stacktrace is rendered.
unicode_frames(_Config) ->
    {C,R,Stk0} = capture(fun() -> erlang:error(boom) end),
    Extra = [{'модуль', 'функция', 2, [{file,"файл.erl"},{line,N}]}
             || N <- lists:seq(1, 5)],
    Stk = Stk0 ++ Extra,
    Str = fmt(C, R, Stk),
    true = is_chardata(Str),
    %% The unicode file/function names must survive intact as UTF-8
    %% (not double-encoded) through the accumulated stacktrace binary.
    F = flat(Str),
    true = contains(F, "файл.erl"),
    true = contains(F, "функция"),
    report_mem(unicode_frames, {C,R,Stk}).

%% Build the reported crash shape for N proplist instances, returning
%% the captured {Class, Reason, Stacktrace}.
huge_crash(N) ->
    Instances =
        [{{dummy_key, list_to_atom("item_" ++ integer_to_list(I))},
          [{counter_a, 0}, {counter_b, 0}, {counter_c, 0}, {counter_d, 0},
           {counter_e, 0}, {counter_f, 0}, {counter_g, 0}, {counter_h, 0},
           {counter_i, 0}, {counter_j, 0}, {counter_k, 0}, {counter_l, 0},
           {counter_m, 0}, {counter_n, 0}]}
         || I <- lists:seq(1, N)],
    State = {dummy_state, "dummy_name", Instances, [], undefined, undefined,
             false, 300000000, 1, 1, undefined, undefined, undefined,
             undefined, undefined, undefined},
    %% Mirror the reported crash: function_clause with a custom arg
    %% list whose second element is the huge state.
    capture(fun() ->
                    erlang:error(function_clause,
                                 [{'EXIT', self(), dummy_reason}, State])
            end).

%%%
%%% Formatting helpers
%%%

fmt(Class, Reason, Stk) ->
    erl_error:format_exception(Class, Reason, Stk, #{}).

%% Run Fun, capture the exception as {Class, Reason, Stacktrace}.
capture(Fun) ->
    try Fun() of
        _ -> ct:fail(no_exception_raised)
    catch
        Class:Reason:Stk -> {Class, Reason, Stk}
    end.

flat(Chardata) ->
    unicode:characters_to_list(Chardata).

is_chardata(Chardata) ->
    case unicode:characters_to_list(Chardata) of
        L when is_list(L) -> true;
        _ -> false
    end.

contains(Str, Sub) ->
    string:find(Str, Sub) =/= nomatch.

%% Identity function used to hide values from compile-time analysis
%% so that intended runtime exceptions are not optimized away or
%% flagged as compile warnings.
id(X) -> X.

%%%
%%% Memory reporting framework
%%%

report_mem(Name, Exc) ->
    report_mem(Name, Exc, #{}).

report_mem(Name, Exc, Opts) ->
    report_mem(Name, Exc, Opts, ?ITERATIONS).

report_mem(Name, {Class, Reason, Stk}, Opts, Iterations) ->
    %% Run the measurement over ?TRIALS independent trials. Each trial
    %% formats Iterations times on an isolated worker while tracing
    %% garbage collection, and returns one representative result for
    %% the retained-size metrics. Transient allocation is averaged
    %% across trials (GC timing makes a single trial noisy); we also
    %% report the minimum, which is the most stable lower bound.
    Trials =
        [begin
             {AllocWords, R} = trace_alloc(Class, Reason, Stk, Opts, Iterations),
             {AllocWords / Iterations, R}
         end || _ <- lists:seq(1, ?TRIALS)],
    {PerCallList, [Result|_]} = {[A || {A,_} <- Trials], [R || {_,R} <- Trials]},
    AllocMean = lists:sum(PerCallList) / length(PerCallList),
    AllocMin = lists:min(PerCallList),

    SizeWords = erts_debug:size(Result),
    FlatWords = erts_debug:flat_size(Result),
    Bytes = iolist_size(Result),
    %% Honest total memory: on-heap words (x8 bytes) plus the contents
    %% of off-heap reference-counted binaries (> 64 bytes), which
    %% erts_debug:size does not count. See total_size/1.
    TotalBytes = total_size(Result),

    ct:log("erl_error ~p memory:~n"
           "  result erts_debug:size      = ~p words~n"
           "  result erts_debug:flat_size = ~p words~n"
           "  result iolist_size          = ~p bytes~n"
           "  result total_size           = ~p bytes (on-heap + refc binaries)~n"
           "  transient alloc / call      = ~.1f words mean, ~.1f min"
           " (~p iterations x ~p trials)~n",
           [Name, SizeWords, FlatWords, Bytes, TotalBytes,
            AllocMean, AllocMin, Iterations, ?TRIALS]),

    {comment,
     lists:flatten(
       io_lib:format("size=~pw total=~pB bytes=~p alloc/call=~.1fw(mean) ~.1fw(min)",
                     [SizeWords, TotalBytes, Bytes, AllocMean, AllocMin]))}.

%% Honest total memory of a formatted result (nested iodata) in bytes.
%%
%% erts_debug:size/1 counts on-heap words but NOT the contents of
%% reference-counted (off-heap) binaries (those > 64 bytes) — only
%% their small on-heap ProcBin header. This adds those bytes back so
%% the figure reflects total memory, not just process-heap footprint.
total_size(Term) ->
    erts_debug:size(Term) * erlang:system_info(wordsize) + refc_bytes(Term).

refc_bytes(Bin) when is_binary(Bin), byte_size(Bin) > 64 ->
    byte_size(Bin);
refc_bytes([H | T]) ->
    refc_bytes(H) + refc_bytes(T);
refc_bytes(_) ->
    0.

%% Measure heap reclaimed by the garbage collector on a dedicated
%% worker process running the format N times. The reclaimed amount is
%% a proxy for the transient garbage produced while formatting. The
%% worker also returns one formatted result so the caller can measure
%% its retained size without formatting a second time.
trace_alloc(Class, Reason, Stk, Opts, N) ->
    {Pid, Ref} =
        spawn_monitor(?MODULE, format_worker,
                      [self(), {Class, Reason, Stk, Opts, N}]),
    %% Trace GC on the worker; events are delivered to this process.
    erlang:trace(Pid, true, [garbage_collection, {tracer, self()}]),
    Pid ! go,
    Result = receive {result, Pid, R} -> R end,
    Reclaimed = collect_gc(Pid, Ref, 0),
    {Reclaimed, Result}.

format_worker(Parent, {Class, Reason, Stk, Opts, N}) ->
    MRef = erlang:monitor(process, Parent),
    receive
        go ->
            erlang:garbage_collect(),
            Result = loop_format(Class, Reason, Stk, Opts, N),
            %% Send the last result back for size measurement. It is
            %% copied to the parent heap, which does not affect its
            %% erts_debug:size/flat_size.
            Parent ! {result, self(), Result},
            erlang:garbage_collect(),
            ok;
        {'DOWN', MRef, process, Parent, _} ->
            ok
    end.

loop_format(C, R, S, O, 1) ->
    %% Keep the final result so it can be returned to the parent.
    Result = erl_error:format_exception(C, R, S, O),
    _ = erlang:iolist_size(Result),
    Result;
loop_format(C, R, S, O, N) when N > 1 ->
    %% Force the result to be built; ignore it so it becomes garbage.
    _ = erlang:iolist_size(erl_error:format_exception(C, R, S, O)),
    loop_format(C, R, S, O, N - 1).

%% Sum heap_size deltas between gc_*_start and gc_*_end events. The
%% difference (start heap - end heap) is the space reclaimed by that
%% collection, i.e. garbage that had accumulated.
collect_gc(Pid, Ref, Acc) ->
    receive
        {trace, Pid, gc_minor_start, Info} ->
            collect_gc(Pid, Ref, {Acc, heap_words(Info)});
        {trace, Pid, gc_major_start, Info} ->
            collect_gc(Pid, Ref, {Acc, heap_words(Info)});
        {trace, Pid, gc_minor_end, Info} ->
            collect_gc(Pid, Ref, add_reclaimed(Acc, Info));
        {trace, Pid, gc_major_end, Info} ->
            collect_gc(Pid, Ref, add_reclaimed(Acc, Info));
        {trace, Pid, gc_start, Info} ->
            collect_gc(Pid, Ref, {Acc, heap_words(Info)});
        {trace, Pid, gc_end, Info} ->
            collect_gc(Pid, Ref, add_reclaimed(Acc, Info));
        {'DOWN', Ref, process, Pid, _} ->
            case Acc of
                {N, _Pending} -> N;
                N when is_integer(N) -> N
            end
    end.

add_reclaimed({Acc, StartHeap}, EndInfo) when is_integer(Acc) ->
    EndHeap = heap_words(EndInfo),
    Reclaimed = max(0, StartHeap - EndHeap),
    Acc + Reclaimed;
add_reclaimed(Acc, _EndInfo) when is_integer(Acc) ->
    %% end without a matching start; ignore
    Acc.

heap_words(Info) ->
    HeapSize = proplists:get_value(heap_size, Info, 0),
    OldHeapSize = proplists:get_value(old_heap_size, Info, 0),
    MbufSize = proplists:get_value(mbuf_size, Info, 0),
    HeapSize + OldHeapSize + MbufSize.
