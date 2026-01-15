%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

-module(trace_session_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

-export([test_set_on_spawn/1,
         test_set_on_first_spawn/1,
         test_set_on_link/1,
         test_set_on_first_link/1,
         on_load/1,
         trace_info_on_load/1,
         procs/1,
         basic/1,
         call/1,
         meta/1,
         ms_enable_flags/1,
         return_to/1,
         system_monitor_info/1,
         system_monitor_long_msgq/1,
         destroy/1,
         negative/1,
         error_info/1,
         timem_basic/1,
         is_bif_traced/1,

         end_of_list/1]).

-include_lib("common_test/include/ct.hrl").


-undef(line).
-ifdef(debug).
-define(line,io:format("line ~p\n",[?LINE]),erlang:display(?LINE)).
-else.
-define(line,void).
-endif.

-export([foo/0, exported/1, middle/1, bottom/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [
     basic,
     call,
     meta,
     ms_enable_flags,
     on_load,
     trace_info_on_load,
     procs,
     test_set_on_spawn,
     test_set_on_first_spawn,
     test_set_on_link,
     test_set_on_first_link,
     return_to,
     system_monitor_info,
     system_monitor_long_msgq,
     destroy,
     negative,
     error_info,
     timem_basic,
     is_bif_traced,

     end_of_list].

init_per_suite(Config) ->
    Was = erts_debug:set_internal_state(available_internal_state, true),
    [{available_internal_state, Was} | Config].

end_per_suite(Config) ->
    case proplists:get_value(available_internal_state, Config) of
        false ->
            erts_debug:set_internal_state(available_internal_state, false);
        true ->
            ignore
    end.

erlang_trace(legacy, Pid, How, FlagList) ->
    erlang:trace(Pid, How, FlagList);
erlang_trace(Session, Pid, How, FlagList) when is_pid(Pid) ->
    trace:process(Session, Pid, How, FlagList).

erlang_trace_pattern(legacy, MFA, MS, FlagList) ->
    erlang:trace_pattern(MFA, MS, FlagList);
erlang_trace_pattern(Session, MFA, MS, FlagList0) ->
    FlagList1 = lists:keydelete(tracer, 1, FlagList0),
    FlagList3 = case lists:keytake(meta, 1, FlagList1) of
                    {value, {meta,_Tracer}, FlagList2} ->
                        [meta | FlagList2];
                    false ->
                        FlagList1
                end,
    trace:function(Session, MFA, MS, FlagList3).


on_load(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    catch erlang:purge_module(dummy),
    erlang:delete_module(dummy),
    catch erlang:purge_module(dummy),

    on_load1(S1, Tracer1),
    on_load2(S1, Tracer1, [],
             S2, Tracer2, []),
    on_load2(legacy, Tracer0, [{tracer,Tracer0}],
             S2, Tracer2, []),
    on_load2(S2, Tracer2, [],
             legacy, Tracer0, [{tracer,Tracer0}]),

    on_load2(S1, Tracer1, [],
             S0, Tracer0, [{tracer,Tracer0}]),
    on_load2(S0, Tracer0, [{tracer,Tracer0}],
             S1, Tracer1, []),

    ok.

trace_info_on_load(_Config) ->
    Tester = self(),
    Tracer = spawn_link(fun() -> tracer("Tracer", Tester) end),
    S = trace:session_create(?MODULE, Tracer, []),
    1 = trace:process(S, self(), true, [call]),
    0 = trace:function(S, on_load, true, []),
    {all,[{traced,global},
      {match_spec,[]},
      {meta,false},
      {meta_match_spec,false},
      {call_count,false}]} = trace:info(S, on_load, all),
    ok.

on_load1(Session, Tracer) ->
    Tracee = self(),
    1 = trace:process(Session, self(), true, [call]),
    0 = trace:function(Session, on_load, true, []),
    dummy:module_info(),
    erlang:delete_module(dummy),
    erlang:purge_module(dummy),
    ?line,
    {Tracer, {trace,Tracee,call,{dummy,module_info,[]}}} = receive_any(),
    1 = trace:process(Session, self(), false, [call]),
    dummy:module_info(),
    erlang:delete_module(dummy),
    erlang:purge_module(dummy),
    ?line,
    timeout = receive_nothing(),
    0 = trace:function(Session, on_load, false, []),
    ok.
on_load2(S1, Tracer1, Opts1, S2, Tracer2, Opts2) ->
    Tracee = self(),
    1 = erlang_trace(S1, self(), true, [call|Opts1]),
    1 = erlang_trace(S2, self(), true, [call|Opts2]),
    Opts1_1 = proplists:delete(tracer, Opts1),
    Opts2_1 = proplists:delete(tracer, Opts2),
    0 = erlang_trace_pattern(S1, on_load, true, Opts1_1),
    0 = erlang_trace_pattern(S2, on_load, true, Opts2_1),
    dummy:module_info(),
    erlang:delete_module(dummy),
    erlang:purge_module(dummy),
    ?line,
    receive_unsorted(
        [{Tracer1,{trace,Tracee,call,{dummy,module_info,[]}}},
         {Tracer2,{trace,Tracee,call,{dummy,module_info,[]}}}]),
    1 = erlang_trace(S1, self(), false, [call|Opts1]),
    dummy:module_info(),
    erlang:delete_module(dummy),
    erlang:purge_module(dummy),
    ?line,
    {Tracer2, {trace,Tracee,call,{dummy,module_info,[]}}} = receive_any(),
    1 = erlang_trace(S2, self(), false, [call|Opts2]),
    dummy:module_info(),
    erlang:delete_module(dummy),
    erlang:purge_module(dummy),
    ?line,
    timeout = receive_nothing(),

    0 = erlang_trace_pattern(S1, on_load, false, Opts1_1),
    0 = erlang_trace_pattern(S2, on_load, false, Opts2_1),
    ok.

test_set_on_spawn(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    set_on_spawn(S1, Tracer1),
    set_on_spawn2(S1, Tracer1, [],
                  S2, Tracer2, []),
    set_on_spawn2(legacy, Tracer0, [{tracer,Tracer0}],
                  S2, Tracer2, []),
    set_on_spawn2(S2, Tracer2, [],
                  legacy, Tracer0, [{tracer,Tracer0}]),

    set_on_spawn2(S1, Tracer1, [],
                  S0, Tracer0, [{tracer,Tracer0}]),
    set_on_spawn2(S0, Tracer0, [{tracer,Tracer0}],
                  S1, Tracer1, []),

    true = trace:session_destroy(S0),
    true = trace:session_destroy(S1),
    true = trace:session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),
    ok.
test_set_on_first_spawn(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    set_on_first_spawn(S1, Tracer1),
    set_on_first_spawn2(S1, Tracer1, [],
                        S2, Tracer2, []),
    set_on_first_spawn2(legacy, Tracer0, [{tracer,Tracer0}],
                        S2, Tracer2, []),
    set_on_first_spawn2(S2, Tracer2, [],
                        legacy, Tracer0, [{tracer,Tracer0}]),

    set_on_first_spawn2(S1, Tracer1, [],
                        S0, Tracer0, [{tracer,Tracer0}]),
    set_on_first_spawn2(S0, Tracer0, [{tracer,Tracer0}],
                        S1, Tracer1, []),

    true = trace:session_destroy(S0),
    true = trace:session_destroy(S1),
    true = trace:session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),
    ok.
test_set_on_link(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    set_on_link(S1, Tracer1),
    set_on_link2(S1, Tracer1, [],
                 S2, Tracer2, []),
    set_on_link2(legacy, Tracer0, [{tracer,Tracer0}],
                 S2, Tracer2, []),
    set_on_link2(S2, Tracer2, [],
                 legacy, Tracer0, [{tracer,Tracer0}]),

    set_on_link2(S1, Tracer1, [],
                 S0, Tracer0, [{tracer,Tracer0}]),
    set_on_link2(S0, Tracer0, [{tracer,Tracer0}],
                 S1, Tracer1, []),
    true = trace:session_destroy(S0),
    true = trace:session_destroy(S1),
    true = trace:session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),
    ok.

test_set_on_first_link(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    set_on_first_link(S1, Tracer1),
    set_on_first_link2(S1, Tracer1, [],
                       S2, Tracer2, []),
    set_on_first_link2(legacy, Tracer0, [{tracer,Tracer0}],
                       S2, Tracer2, []),
    set_on_first_link2(S2, Tracer2, [],
                       legacy, Tracer0, [{tracer,Tracer0}]),

    set_on_first_link2(S1, Tracer1, [],
                       S0, Tracer0, [{tracer,Tracer0}]),
    set_on_first_link2(S0, Tracer0, [{tracer,Tracer0}],
                       S1, Tracer1, []),
    true = trace:session_destroy(S0),
    true = trace:session_destroy(S1),
    true = trace:session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),
    ok.
set_on_spawn(Session, Tracer) ->
    1 = trace:process(Session, self(), true, [procs, set_on_spawn]),
    Tracee = self(),
    Child = spawn(fun F() ->
                          receive hej ->
                                  Pid = spawn(fun() -> receive M -> M end end),
                                  exit(Pid, die),
                                  Tracee ! {done, Pid},
                                  F()
                          end
                  end),
    Child ! hej,
    receive {done, GrandChild} -> ok end,
    exit(Child, die),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn, Child,'_'}}],

       [{Tracer,{trace, Child, spawned, Tracee,'_'}},
        {Tracer,{trace, Child, spawn, GrandChild, '_'}},
        {Tracer,{trace, Child, exit, die}}],

       [{Tracer,{trace, GrandChild, spawned, Child,'_'}},
        {Tracer,{trace, GrandChild, exit, die}}]
      }),
    1 = trace:process(Session, self(), false, [procs, set_on_spawn]),
    ok.

set_on_spawn2(S1, Tracer1, Opts1, S2, Tracer2, Opts2) ->
    1 = erlang_trace(S1, self(), true, [procs, set_on_spawn | Opts1]),
    1 = erlang_trace(S2, self(), true, [procs, set_on_spawn | Opts2]),
    Tracee = self(),
    Child = spawn(fun F() ->
                          receive hej ->
                                  Pid = spawn(fun() -> receive M -> M end end),
                                  exit(Pid, die),
                                  Tracee ! {done, Pid},
                                  F()
                          end
                  end),
    Child ! hej,
    receive {done, GrandChild} -> ok end,
    exit(Child, die),
    receive_parallel(
      {[{Tracer1, {trace, Tracee, spawn, Child,'_'}}],

       [{Tracer2, {trace, Tracee, spawn, Child,'_'}}],

       [{Tracer1, {trace, Child, spawned, Tracee, '_'}},
        {Tracer1, {trace, Child, spawn, GrandChild, '_'}},
        {Tracer1, {trace, Child, exit, die}}],

       [{Tracer2, {trace, Child, spawned, Tracee,'_'}},
        {Tracer2, {trace, Child, spawn, GrandChild, '_'}},
        {Tracer2, {trace, Child, exit, die}}],

       [{Tracer1, {trace, GrandChild, spawned, Child, '_'}},
        {Tracer1, {trace, GrandChild, exit, die}}],

       [{Tracer2, {trace, GrandChild, spawned, Child, '_'}},
        {Tracer2, {trace, GrandChild, exit, die}}]
      }),

    1 = erlang_trace(S1, self(), false, [procs, set_on_spawn | Opts1]),

    Child1 = spawn(fun F() ->
                           receive hej ->
                                   Pid = spawn(fun() -> receive M -> M end end),
                                   exit(Pid, die),
                                   Tracee ! {done, Pid},
                                   F()
                           end
                   end),
    Child1 ! hej,
    receive {done, GrandChild1} -> ok end,
    exit(Child1, die),
    receive_parallel(
      {[{Tracer2, {trace, Tracee, spawn, Child1,'_'}}],

       [{Tracer2, {trace, Child1, spawned, Tracee, '_'}},
        {Tracer2, {trace, Child1, spawn, GrandChild1, '_'}},
        {Tracer2, {trace, Child1, exit, die}}],

       [{Tracer2,{trace, GrandChild1, spawned, Child1,'_'}},
        {Tracer2,{trace, GrandChild1, exit, die}}]
      }),

    1 = erlang_trace(S2, self(), false, [procs, set_on_spawn | Opts2]),
    ok.

set_on_first_spawn(Session, Tracer) ->
    1 = trace:process(Session, self(), true, [procs, set_on_first_spawn]),
    Tracee = self(),
    TraceeChild = spawn(fun F() ->
                                receive hej ->
                                        Pid = spawn(fun() -> receive M -> M end end),
                                        Tracee ! spawned,
                                        exit(Pid, die),
                                        F()
                                end
                        end),
    TraceeChild ! hej,
    receive spawned -> ok end,
    exit(TraceeChild, die),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    exit(TraceeChild1, die),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn,TraceeChild,'_'}},
        {Tracer,{trace, Tracee, spawn,TraceeChild1,'_'}}],

       [{Tracer,{trace, TraceeChild, spawned,Tracee,'_'}},
        {Tracer,{trace, TraceeChild, spawn,'_','_'}},
        {Tracer,{trace, TraceeChild, exit, die}}]}),

    1 = trace:process(Session, self(), false, [procs, set_on_first_spawn]),
    ok.
set_on_first_spawn2(S1, Tracer1, Opts1, S2, Tracer2, Opts2) ->
    1 = erlang_trace(S1, self(), true, [procs, set_on_first_spawn | Opts1]),
    1 = erlang_trace(S2, self(), true, [procs, set_on_first_spawn | Opts2]),
    Tracee = self(),
    TraceeChild = spawn(fun F() ->
                                receive hej ->
                                        Pid = spawn(fun() -> receive M -> M end end),
                                        exit(Pid, die),
                                        Tracee ! done,
                                        F()
                                end
                        end),
    TraceeChild ! hej,
    receive done -> ok end,
    exit(TraceeChild, die),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    exit(TraceeChild1, die),
    receive_parallel(
      {[{Tracer1,{trace, Tracee, spawn,TraceeChild,'_'}},
        {Tracer1,{trace, Tracee, spawn,TraceeChild1,'_'}}],

       [{Tracer2,{trace, Tracee, spawn,TraceeChild,'_'}},
        {Tracer2,{trace, Tracee, spawn,TraceeChild1,'_'}}],

       [{Tracer1,{trace, TraceeChild, spawned,Tracee,'_'}},
        {Tracer1,{trace, TraceeChild, spawn,'_','_'}},
        {Tracer1,{trace, TraceeChild, exit, die}}],

       [{Tracer2,{trace, TraceeChild, spawned,Tracee,'_'}},
        {Tracer2,{trace, TraceeChild, spawn,'_','_'}},
        {Tracer2,{trace, TraceeChild, exit, die}}]
      }),

    1 = erlang_trace(S1, self(), false, [procs, set_on_first_spawn | Opts1]),
    TraceeChild2 = spawn(fun F() ->
                                 receive hej ->
                                         Pid = spawn(fun() -> receive M -> M end end),
                                         exit(Pid, die),
                                         F()
                                 end
                         end),
    TraceeChild2 ! hej,
    exit(TraceeChild2, die),
    TraceeChild21 = spawn(fun() -> receive M -> M end end),
    exit(TraceeChild21, die),

    {Tracer2,{trace, Tracee, spawn,TraceeChild2,_}} = receive_any(),
    {Tracer2,{trace, Tracee, spawn,TraceeChild21,_}} = receive_any(),
    1 = erlang_trace(S2, self(), false, [procs, set_on_first_spawn | Opts2]),
    ok.

set_on_link(Session, Tracer) ->
    %% Test set_on_link via spawn_link/1
    1 = trace:process(Session, self(), true, [procs, set_on_link]),
    Tracee = self(),
    Child = spawn_link(fun() -> receive M -> M end end),
    unlink(Child),
    exit(Child, die),
    receive_parallel(
      {[{Tracer, {trace, Tracee, spawn, Child,'_'}},
        {Tracer, {trace, Tracee, link, Child}},
        {Tracer, {trace, Tracee, unlink, Child}}],

       [{Tracer, {trace, Child, spawned,Tracee,'_'}},
        {Tracer, {trace, Child, getting_linked, Tracee}},
        {Tracer, {trace, Child, getting_unlinked, Tracee}},
        {Tracer, {trace, Child, exit, die}}]
      }),

    %% Test set_on_link via link/1
    Child2 = spawn(fun() -> receive M -> M end end),
    link(Child2),
    unlink(Child2),
    exit(Child2, die),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn,Child2,'_'}},
        {Tracer,{trace, Tracee, link, Child2}},
        {Tracer,{trace, Tracee, unlink, Child2}}],

       [{Tracer,{trace, Child2, getting_linked, Tracee}},
        {Tracer,{trace, Child2, getting_unlinked, Tracee}},
        {Tracer,{trace, Child2, exit, die}}]
       }),

    %% Test that you can disable the tracer but still get traces
    %% from child process
    Child3 = spawn(fun() -> receive M -> M end end),
    link(Child3),
    1 = trace:process(Session, self(), false, [procs, set_on_link]),
    unlink(Child3),
    exit(Child3, die),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn, Child3,'_'}},
        {Tracer,{trace, Tracee, link, Child3}}],

       [{Tracer,{trace, Child3, getting_linked, Tracee}},
        {Tracer,{trace, Child3, getting_unlinked, Tracee}},
        {Tracer,{trace, Child3, exit, die}}]
      }),

    %% Test that you can disable the tracer for the child process
    %% and get messages from the parent
    1 = trace:process(Session, self(), true, [procs, set_on_link]),
    Child4 = spawn(fun() -> receive M -> M end end),
    link(Child4),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn,Child4,'_'}},
        {Tracer,{trace, Tracee, link, Child4}}],

       [{Tracer,{trace, Child4, getting_linked, Tracee}}]
      }),
    1 = trace:process(Session, Child4, false, [procs, set_on_link]),
    unlink(Child4),
    exit(Child4, die),
    {Tracer,{trace, Tracee, unlink, Child4}} = receive_any(),

    1 = trace:process(Session, self(), false, [procs, set_on_link]),
    spawn_link(fun() -> receive M -> M end end),
    timeout = receive_nothing(),
    ok.
set_on_link2(S1, Tracer1, Opts1, S2, Tracer2, Opts2) ->
    %% Test multiple tracers with set_on_link via spawn_link/1
    1 = erlang_trace(S1, self(), true, [procs, set_on_link | Opts1]),
    1 = erlang_trace(S2, self(), true, [procs, set_on_link | Opts2]),
    Tracee = self(),
    Child = spawn_link(fun() -> receive M -> M end end),
    unlink(Child),
    exit(Child, die),

    receive_parallel(
      {[{Tracer1,{trace, Tracee, spawn,Child,'_'}},
        {Tracer1,{trace, Tracee, link, Child}},
        {Tracer1,{trace, Tracee, unlink, Child}}],

       [{Tracer2,{trace, Tracee, spawn,Child,'_'}},
        {Tracer2,{trace, Tracee, link, Child}},
        {Tracer2,{trace, Tracee, unlink, Child}}],

       [{Tracer1,{trace, Child, spawned,Tracee,'_'}},
        {Tracer1,{trace, Child, getting_linked, Tracee}},
        {Tracer1,{trace, Child, getting_unlinked, Tracee}},
        {Tracer1,{trace, Child, exit, die}}],

       [{Tracer2,{trace, Child, spawned,Tracee,'_'}},
        {Tracer2,{trace, Child, getting_linked, Tracee}},
        {Tracer2,{trace, Child, getting_unlinked, Tracee}},
        {Tracer2,{trace, Child, exit, die}}]
      }),

    %% Test multiple tracers with set_on_link via link/1
    Child1 = spawn(fun() -> receive M -> M end end),
    ?line,
    link(Child1),
    unlink(Child1),
    exit(Child1, die),

    receive_parallel(
      {[{Tracer1,{trace, Tracee, spawn,Child1,'_'}},
        {Tracer1,{trace, Tracee, link, Child1}},
        {Tracer1,{trace, Tracee, unlink, Child1}}],

       [{Tracer2,{trace, Tracee, spawn,Child1,'_'}},
        {Tracer2,{trace, Tracee, link, Child1}},
        {Tracer2,{trace, Tracee, unlink, Child1}}],

       [{Tracer1,{trace, Child1, getting_linked, Tracee}},
        {Tracer1,{trace, Child1, getting_unlinked, Tracee}},
        {Tracer1,{trace, Child1, exit, die}}],

       [{Tracer2,{trace, Child1, getting_linked, Tracee}},
        {Tracer2,{trace, Child1, getting_unlinked, Tracee}},
        {Tracer2,{trace, Child1, exit, die}}]
      }),

    %% Disable one tracer and test that set_on_link still works for the other tracer
    %% via spawn_link/1
    1 = erlang_trace(S1, self(), false, [procs, set_on_link | Opts1]),
    Child2 = spawn_link(fun() -> receive M -> M end end),
    unlink(Child2),
    exit(Child2, die),
    receive_parallel(
      {[{Tracer2,{trace, Tracee, spawn,Child2,'_'}},
        {Tracer2,{trace, Tracee, link, Child2}},
        {Tracer2,{trace, Tracee, unlink, Child2}}],

       [{Tracer2,{trace, Child2, spawned,Tracee,'_'}},
        {Tracer2,{trace, Child2, getting_linked, Tracee}},
        {Tracer2,{trace, Child2, getting_unlinked, Tracee}},
        {Tracer2,{trace, Child2, exit, die}}]
      }),
    %% Test with link/1
    Child21 = spawn(fun() -> receive M -> M end end),
    link(Child21),
    unlink(Child21),
    exit(Child21, die),
    receive_parallel(
      {[{Tracer2,{trace, Tracee, spawn,Child21,'_'}},
        {Tracer2,{trace, Tracee, link, Child21}},
        {Tracer2,{trace, Tracee, unlink, Child21}}],

       [{Tracer2,{trace, Child21, getting_linked, Tracee}},
        {Tracer2,{trace, Child21, getting_unlinked, Tracee}},
        {Tracer2,{trace, Child21, exit, die}}]
      }),
    %% Test that you can disable one of the tracers on the child process
    1 = erlang_trace(S1, self(), true, [procs, set_on_link | Opts1]),
    Child11 = spawn(fun() -> receive M -> M end end),
    link(Child11),
    1 = erlang_trace(S1, Child11, false, [procs, set_on_link | Opts1]),
    receive_parallel(
      {[{Tracer1,{trace, Tracee, spawn,Child11,'_'}},
        {Tracer1,{trace, Tracee, link, Child11}}],

       [{Tracer2,{trace, Tracee, spawn,Child11,'_'}},
        {Tracer2,{trace, Tracee, link, Child11}}],

       [{Tracer1,{trace, Child11, getting_linked, Tracee}}],

       [{Tracer2,{trace, Child11, getting_linked, Tracee}}]
      }),
    1 = erlang_trace(S1, Child11, false, [procs, set_on_link | Opts1]),
    unlink(Child11),
    exit(Child11, die),
    receive_parallel(
      {[{Tracer1,{trace, Tracee, unlink, Child11}}],

       [{Tracer2,{trace, Tracee, unlink, Child11}}],

       [{Tracer2,{trace, Child11, getting_unlinked, Tracee}},
        {Tracer2,{trace, Child11, exit, die}}]
      }),

    1 = erlang_trace(S1, self(), false, [procs, set_on_link | Opts1]),
    1 = erlang_trace(S2, self(), false, [procs, set_on_link | Opts2]),
    ok.

set_on_first_link(Session, Tracer) ->
    1 = trace:process(Session, self(), true, [procs, set_on_first_link]),
    Tracee = self(),
    Child = spawn_link(fun() -> receive M -> M end end),
    Child1 = spawn_link(fun() -> receive M -> M end end),
    unlink(Child),
    exit(Child, die),
    unlink(Child1),
    exit(Child1, die),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn,Child,'_'}},
        {Tracer,{trace, Tracee, link, Child}},
        {Tracer,{trace, Tracee, spawn,Child1,'_'}},
        {Tracer,{trace, Tracee, link, Child1}},
        {Tracer,{trace, Tracee, unlink, Child}},
        {Tracer,{trace, Tracee, unlink, Child1}}],

       [{Tracer,{trace, Child, spawned,Tracee,'_'}},
        {Tracer,{trace, Child, getting_linked, Tracee}},
        {Tracer,{trace, Child, getting_unlinked, Tracee}},
        {Tracer,{trace, Child, exit, die}}]
      }),
    %% Test that spawn on first link works with link/1
    1 = trace:process(Session, Tracee, true, [set_on_first_link]),
    Child2 = spawn(fun() -> receive M -> M end end),
    Child21 = spawn(fun() -> receive M -> M end end),
    link(Child2),
    link(Child21),
    unlink(Child2),
    unlink(Child21),
    exit(Child2, die),
    exit(Child21,die),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn,Child2,'_'}},
        {Tracer,{trace, Tracee, spawn,Child21,'_'}},
        {Tracer,{trace, Tracee, link, Child2}},
        {Tracer,{trace, Tracee, link, Child21}},
        {Tracer,{trace, Tracee, unlink, Child2}},
        {Tracer,{trace, Tracee, unlink, Child21}}],

       [{Tracer,{trace, Child2, getting_linked, Tracee}},
        {Tracer,{trace, Child2, getting_unlinked, Tracee}},
        {Tracer,{trace, Child2, exit, die}}]
      }),
    %% Test that spawn on first link flag is not on the child
    1 = trace:process(Session, Tracee, true, [set_on_first_link]),
    Child3 = spawn(fun F() -> receive hej ->
                                      spawn_link(fun()-> receive M -> M end end),
                                      Tracee ! done,
                                      F()
                              end
                   end),
    link(Child3),
    Child3 ! hej,
    receive done -> ok end,
    unlink(Child3),
    exit(Child3, die),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn,Child3,'_'}},
        {Tracer,{trace, Tracee, link, Child3}},
        {Tracer,{trace, Tracee, unlink, Child3}}],

       [{Tracer,{trace, Child3, getting_linked, Tracee}},
        {Tracer,{trace, Child3, spawn,'_','_'}},
        {Tracer,{trace, Child3, link, '_'}},
        {Tracer,{trace, Child3, getting_unlinked, Tracee}},
        {Tracer,{trace, Child3, exit, die}}]
      }),
    1 = trace:process(Session, self(), false, [procs, set_on_first_link]),
    ok.

set_on_first_link2(S1, Tracer1, Opts1, S2, Tracer2, Opts2) ->
    1 = erlang_trace(S1, self(), true, [procs, set_on_first_link | Opts1]),
    1 = erlang_trace(S2, self(), true, [procs, set_on_first_link | Opts2]),
    Tracee = self(),
    Child = spawn_link(fun() -> receive M -> M end end),
    Child1 = spawn_link(fun() -> receive M -> M end end),
    receive_parallel(
      {[{Tracer1,{trace, Tracee, spawn,Child,'_'}},
        {Tracer1,{trace, Tracee, link, Child}},
        {Tracer1,{trace, Tracee, spawn,Child1,'_'}},
        {Tracer1,{trace, Tracee, link, Child1}}],

       [{Tracer2,{trace, Tracee, spawn,Child,'_'}},
        {Tracer2,{trace, Tracee, link, Child}},
        {Tracer2,{trace, Tracee, spawn,Child1,'_'}},
        {Tracer2,{trace, Tracee, link, Child1}}],

       [{Tracer1,{trace, Child, spawned,Tracee,'_'}},
        {Tracer1,{trace, Child, getting_linked, Tracee}}],

       [{Tracer2,{trace, Child, spawned,Tracee,'_'}},
        {Tracer2,{trace, Child, getting_linked, Tracee}}]
       }),

    unlink(Child),
    exit(Child, die),
    unlink(Child1),
    exit(Child1, die),
    receive_parallel(
      {[{Tracer1,{trace, Tracee, unlink, Child}},
        {Tracer1,{trace, Tracee, unlink, Child1}}],

       [{Tracer2,{trace, Tracee, unlink, Child}},
        {Tracer2,{trace, Tracee, unlink, Child1}}],

       [{Tracer1,{trace, Child, getting_unlinked, Tracee}},
        {Tracer1,{trace, Child, exit, die}}],

       [{Tracer2,{trace, Child, getting_unlinked, Tracee}},
        {Tracer2,{trace, Child, exit, die}}]
      }),

    1 = erlang_trace(S1, self(), false, [procs, set_on_first_link | Opts1]),
    Child2 = spawn_link(fun() -> receive M -> M end end),
    Child21 = spawn_link(fun() -> receive M -> M end end),
    unlink(Child2),
    exit(Child2, die),
    unlink(Child21),
    exit(Child21, die),
    {Tracer2,{trace, Tracee, spawn,Child2,_}} = receive_any(),
    {Tracer2,{trace, Tracee, link, Child2}} = receive_any(),
    {Tracer2,{trace, Tracee, spawn,Child21,_}} = receive_any(),
    {Tracer2,{trace, Tracee, link, Child21}} = receive_any(),
    {Tracer2,{trace, Tracee, unlink, Child2}} = receive_any(),
    {Tracer2,{trace, Tracee, unlink, Child21}} = receive_any(),
    1 = erlang_trace(S2, self(), false, [procs, set_on_first_link | Opts2]),
    ok.
procs(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),

    procs_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    procs_do2(S1, Tracer1, [],
              S2, Tracer2, []),
    procs_do2(legacy, Tracer0, [{tracer,Tracer0}],
              S2, Tracer2, []),
    procs_do2(S2, Tracer2, [],
              legacy, Tracer0, [{tracer,Tracer0}]),

    procs_do2(S1, Tracer1, [],
              S0, Tracer0, [{tracer,Tracer0}]),
    procs_do2(S0, Tracer0, [{tracer,Tracer0}],
              S1, Tracer1, []),

    true = trace:session_destroy(S0),
    true = trace:session_destroy(S1),
    true = trace:session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),
    ok.

procs_do1(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    unregister(RegName),
    Child = spawn(fun() -> receive M -> M end end),
    link(Child),
    unlink(Child),
    exit(Child, die),
    receive_parallel(
      {[{Tracer, {trace, Tracee, register, RegName}},
        {Tracer, {trace, Tracee, unregister, RegName}},
        {Tracer, {trace, Tracee, spawn, Child, '_'}},
        {Tracer, {trace, Tracee, link, Child}},
        {Tracer, {trace, Tracee, unlink, Child}}],

       [{Tracer, {trace, Child, spawned, Tracee, '_'}},
        {Tracer, {trace, Child, getting_linked, Tracee}},
        {Tracer, {trace, Child, getting_unlinked, Tracee}},
        {Tracer, {trace, Child, exit, '_'}}]}),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts]),

    register(RegName, Tracee),
    unregister(RegName),
    Child1 = spawn(fun() -> receive M -> M end end),
    link(Child1),
    unlink(Child1),
    exit(Child1, die),

    timeout = receive_nothing(),

    ok.
procs_do2(S1, Tracer1, Opts1, S2, Tracer2, Opts2) ->
    1 = erlang_trace(S1, self(), true, [procs, set_on_spawn | Opts1]),
    1 = erlang_trace(S2, self(), true, [procs, set_on_spawn | Opts2]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    unregister(RegName),
    Child = spawn(fun() -> receive M -> M end end),
    link(Child),
    unlink(Child),
    exit(Child, die),
    receive_parallel(
      {[{Tracer1, {trace, Tracee, register, RegName}},
        {Tracer1, {trace, Tracee, unregister, RegName}},
        {Tracer1, {trace, Tracee, spawn, Child, '_'}},
        {Tracer1, {trace, Tracee, link, Child}},
        {Tracer1, {trace, Tracee, unlink, Child}}],

       [{Tracer2, {trace, Tracee, register, RegName}},
        {Tracer2, {trace, Tracee, unregister, RegName}},
        {Tracer2, {trace, Tracee, spawn, Child, '_'}},
        {Tracer2, {trace, Tracee, link, Child}},
        {Tracer2, {trace, Tracee, unlink, Child}}],

       [{Tracer1, {trace, Child, spawned, Tracee, '_'}},
        {Tracer1, {trace, Child, getting_linked, Tracee}},
        {Tracer1, {trace, Child, getting_unlinked, Tracee}},
        {Tracer1, {trace, Child, exit, '_'}}],

       [{Tracer2, {trace, Child, spawned, Tracee, '_'}},
        {Tracer2, {trace, Child, getting_linked, Tracee}},
        {Tracer2, {trace, Child, getting_unlinked, Tracee}},
        {Tracer2, {trace, Child, exit, '_'}}]
      }),

    1 = erlang_trace(S1, self(), false, [procs, set_on_spawn | Opts1]),
    register(RegName, Tracee),
    unregister(RegName),
    Child1 = spawn(fun() -> receive M -> M end end),
    link(Child1),
    unlink(Child1),
    exit(Child1, die),
    receive_parallel(
      {[{Tracer2, {trace, Tracee, register, RegName}},
        {Tracer2, {trace, Tracee, unregister, RegName}},
        {Tracer2, {trace, Tracee, spawn, Child1, '_'}},
        {Tracer2, {trace, Tracee, link, Child1}},
        {Tracer2, {trace, Tracee, unlink, Child1}}],

       [{Tracer2, {trace, Child1, spawned, Tracee, '_'}},
        {Tracer2, {trace, Child1, getting_linked, Tracee}},
        {Tracer2, {trace, Child1, getting_unlinked, Tracee}},
        {Tracer2, {trace, Child1, exit, '_'}}]
      }),

    1 = erlang_trace(S2, self(), false, [procs, set_on_spawn | Opts2]),
    register(RegName, Tracee),
    unregister(RegName),
    Child2 = spawn(fun() -> receive M -> M end end),
    link(Child2),
    unlink(Child2),
    exit(Child2, die),
    timeout = receive_nothing(),
    ok.

basic(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0",Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2",Tester) end),

    basic_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    basic_do2(S1, Tracer1, [],
              S2, Tracer2, []),
    basic_do2(legacy, Tracer0, [{tracer,Tracer0}],
              S2, Tracer2, []),
    basic_do2(S2, Tracer2, [],
              legacy, Tracer0, [{tracer,Tracer0}]),

    basic_do2(S1, Tracer1, [],
              S0, Tracer0, [{tracer,Tracer0}]),
    basic_do2(S0, Tracer0, [{tracer,Tracer0}],
              S1, Tracer1, []),

    true = trace:session_destroy(S0),
    true = trace:session_destroy(S1),
    true = trace:session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),

    ok.

basic_do1(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs | Opts]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    unregister(RegName),

    {Tracer, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer, {trace, Tracee, unregister, RegName}} = receive_any(),

    1 = erlang:trace(self(), false, [procs | Opts]),

    register(RegName, Tracee),
    unregister(RegName),

    timeout = receive_nothing(),

    ok.

basic_do2(S1, Tracer1, Opts1, S2, Tracer2, Opts2) ->
    1 = erlang_trace(S1, self(), true, [procs | Opts1]),
    1 = erlang_trace(S2, self(), true, [procs | Opts2]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    unregister(RegName),

    receive_parallel({[{Tracer1, {trace, Tracee, register, RegName}},
                       {Tracer1, {trace, Tracee, unregister, RegName}}],

                      [{Tracer2, {trace, Tracee, register, RegName}},
                       {Tracer2, {trace, Tracee, unregister, RegName}}]}),

    1 = erlang_trace(S1, self(), false, [procs | Opts1]),

    register(RegName, Tracee),
    unregister(RegName),

    {Tracer2, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer2, {trace, Tracee, unregister, RegName}} = receive_any(),

    1 = erlang_trace(S2, self(), false, [procs | Opts2]),

    register(RegName, Tracee),
    unregister(RegName),

    timeout = receive_nothing(),

    ok.


call(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0",Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2",Tester) end),

    call_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = trace:session_create(session0, undefined, []),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    [begin
         io:format("CallType = ~p\n", [CallType]),
         call_do2(S1, Tracer1, [],
                  S2, Tracer2, [], CallType),
         call_do2(legacy, Tracer0, [{tracer,Tracer0}],
                  S2, Tracer2, [], CallType),
         call_do2(S2, Tracer2, [],
                  legacy, Tracer0, [{tracer,Tracer0}], CallType),

         call_do2(S1, Tracer1, [],
                  S0, Tracer0, [{tracer,Tracer0}], CallType),
         call_do2(S0, Tracer0, [{tracer,Tracer0}],
                  S1, Tracer1, [], CallType)
     end
     || CallType <- [
                     {[local], fun() -> foo() end},
                     {[global], fun() -> ?MODULE:foo() end},
                     {[local], fun() -> ?MODULE:foo() end}]
    ],

    true = trace:session_destroy(S0),
    true = trace:session_destroy(S1),
    true = trace:session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),

    ok.

call_do1(Tracer, Opts) ->
    MFArity = {?MODULE,foo,0},
    MFArgs = {?MODULE,foo,[]},
    Tracee = self(),
    1 = erlang:trace(Tracee, true, [call | Opts]),
    1 = erlang:trace_pattern(MFArity, true, [local]),

    foo(),

    {Tracer, {trace, Tracee, call, MFArgs}} = receive_any(),

    1 = erlang:trace(self(), false, [call | Opts]),

    foo(),
    timeout = receive_nothing(),

    ok.

call_do2(S1, Tracer1, Opts1, S2, Tracer2, Opts2, {TPopt, Call}) ->
    MFArity = {?MODULE,foo,0},
    MFArgs = {?MODULE,foo,[]},
    Tracee = self(),
    1 = erlang_trace(S1, Tracee, true, [call | Opts1]),
    1 = erlang_trace(S2, Tracee, true, [call | Opts2]),

    ?line,
    1 = erlang_trace_pattern(S1, MFArity, true, TPopt),
    Call(),
    {Tracer1, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace_pattern(S2, MFArity, true, TPopt),
    Call(),
    receive_unsorted([{Tracer1, {trace, Tracee, call, MFArgs}},
                      {Tracer2, {trace, Tracee, call, MFArgs}}]),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace_pattern(S2, MFArity, false, TPopt),
    Call(),
    {Tracer1, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace_pattern(S2, MFArity, true, TPopt),
    Call(),
    receive_unsorted([{Tracer1, {trace, Tracee, call, MFArgs}},
                      {Tracer2, {trace, Tracee, call, MFArgs}}]),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace_pattern(S1, MFArity, false, TPopt),
    Call(),
    {Tracer2, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace_pattern(S1, MFArity, true, TPopt),
    Call(),
    receive_unsorted([{Tracer1, {trace, Tracee, call, MFArgs}},
                      {Tracer2, {trace, Tracee, call, MFArgs}}]),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace(S1, self(), false, [call | Opts1]),
    Call(),
    {Tracer2, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace_pattern(S2, MFArity, false, TPopt),
    Call(),
    timeout = receive_nothing(),

    ?line,
    1 = erlang_trace_pattern(S1, MFArity, false, TPopt),
    1 = erlang_trace(S2, self(), false, [call | Opts2]),
    Call(),
    timeout = receive_nothing(),

    ok.

meta(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0",Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    S1 = trace:session_create(session1, Tracer1, []),

    meta_do(legacy, Tracer0, S1, Tracer1),
    meta_do(S1, Tracer1, legacy, Tracer0),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    ok.

meta_do(S1, Tracer1, S2, Tracer2) ->
    Tester = self(),
    MFArity = {?MODULE,foo,0},
    MFArgs = {?MODULE,foo,[]},

    1 = erlang_trace_pattern(S1, MFArity, true, [{meta,Tracer1}]),
    1 = erlang_trace_pattern(S2, MFArity, true, [{meta,Tracer2}]),

    ?line,
    foo(),
    receive_parallel({[{Tracer1, {trace_ts,Tester,call,MFArgs,{'_','_','_'}}}],
                      [{Tracer2, {trace_ts,Tester,call,MFArgs,{'_','_','_'}}}]}),

    ?line,
    ?MODULE:foo(),
    receive_parallel({[{Tracer1, {trace_ts,Tester,call,MFArgs,{'_','_','_'}}}],
                      [{Tracer2, {trace_ts,Tester,call,MFArgs,{'_','_','_'}}}]}),

    1 = erlang_trace_pattern(S1, MFArity, false, [meta]),

    ?line,
    foo(),
    {Tracer2, {trace_ts,Tester,call,MFArgs,{_,_,_}}} = receive_any(),

    1 = erlang_trace_pattern(S2, MFArity, false, [meta]),
    timeout = receive_nothing(),

    ok.

%% Test that enable trace flags with match spec on untraced process
%% uses session tracer and not tracer of current process.
ms_enable_flags(_Config) ->
    Tester = self(),
    Dummy = spawn_link(fun() -> receive die -> ok end end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    S1 = trace:session_create(session1, Tracer1, []),

    %% Test enable trace flag on current process
    Fun = fun(EnableSend, DisableSend) ->
                  trace:function(S1, {?MODULE,foo,0},
                                 [{'_', [], [EnableSend]}],
                                 [meta]),

                  foo(),
                  {Tracer1, {trace_ts, Tester, call, {?MODULE,foo,[]}, {_,_,_}}}
                      = receive_any(),

                  {flags, [send]} = trace:info(S1, Tester, flags),
                  Dummy ! message,
                  {Tracer1, {trace, Tester, send, message, Dummy}} = receive_any(),

                  trace:function(S1, {?MODULE,foo,0},
                                 [{'_', [], [DisableSend]}],
                                 [meta]),
                  Dummy ! message,
                  {Tracer1, {trace, Tester, send, message, Dummy}} = receive_any(),
                  foo(),
                  {Tracer1, {trace_ts, Tester, call, {?MODULE,foo,[]}, {_,_,_}}}
                      = receive_any(),
                  {flags, []} = trace:info(S1, Tester, flags),
                  timeout = receive_nothing(),
                  ok
          end,
    Fun({trace, [], [send]}, {trace, [send], []}),
    Fun({enable_trace, send}, {disable_trace, send}),

    %% Test enable trace flag on other process
    Other = spawn_link(fun() -> receive die -> ok end end),
    Fun2 = fun(EnableRecv, DisableRecv) ->
                   trace:function(S1, {?MODULE,foo,0},
                                  [{'_', [], [EnableRecv]}],
                                  [meta]),

                  foo(),
                  {Tracer1, {trace_ts, Tester, call, {?MODULE,foo,[]}, {_,_,_}}}
                       = receive_any(),

                  {flags, ['receive']} = trace:info(S1, Other, flags),
                  Other ! message,
                  {Tracer1, {trace, Other, 'receive', message}} = receive_any(),

                  trace:function(S1, {?MODULE,foo,0},
                                 [{'_', [], [DisableRecv]}],
                                 [meta]),
                  Other ! message,
                  {Tracer1, {trace, Other, 'receive', message}} = receive_any(),
                  foo(),
                  {Tracer1, {trace_ts, Tester, call, {?MODULE,foo,[]}, {_,_,_}}}
                       = receive_any(),
                  {flags, []} = trace:info(S1, Other, flags),
                  timeout = receive_nothing(),

                  ok
          end,
    Fun2({trace, Other, [], ['receive']}, {trace, Other, ['receive'], []}),
    Fun2({enable_trace, Other, 'receive'}, {disable_trace, Other, 'receive'}),

    ok.


return_to(_Config) ->
    %%put(display, true),  %% To get some usable debug printouts

    Tester = self(),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester,get(display)) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2",Tester,get(display)) end),

    Tracee = self(),
    S1 = trace:session_create(session1, Tracer1, []),
    S2 = trace:session_create(session2, Tracer2, []),

    1 = trace:process(S1, Tracee, true, [call, return_to, arity]),
    1 = trace:process(S2, Tracee, true, [call, return_to, arity]),

    [begin
         Traced = [{S1, Tracer1, Funcs1},
                   {S2, Tracer2, Funcs2}],

         %% Set up tracing for all sessions
         [begin
              trace:function(Session, {?MODULE,'_','_'}, false, [local]),
              [begin
                   io_format("trace:function(~p) for tracer ~p\n", [Func, Tracer]),
                   1 = trace:function(Session, {?MODULE,Func,'_'}, true, [local])
               end
               || Func <- TracedFuncs]
          end
          || {Session, Tracer, TracedFuncs} <- Traced],

         %% Execute test with both local and external calls
         [return_to_do(MiddleCall, BottomCall, Traced)
          || MiddleCall <- [local, extern],
             BottomCall <- [local, extern]]
     end
     || Funcs1 <- [[bottom], [middle], [middle,bottom]],
        Funcs2 <- [[bottom], [middle], [middle,bottom]]
    ],

    true = trace:session_destroy(S1),
    ok.

return_to_do(MiddleCall, BottomCall, Traced) ->
    Tracee = self(),
    Script = [{[{'catch',MiddleCall}, {body,BottomCall}, exception],
               #{[bottom] => [{call, bottom}, {return_to, top}],
                 [middle] => [{call, middle}, {return_to, top}],
                 [middle,bottom] => [{call, middle}, {call, bottom}, {return_to,top}]
                }},

              {[{'catch',MiddleCall}, {'catch',BottomCall}, exception],
               #{[bottom] => [{call, bottom}, {return_to, middle}],
                 [middle] => [{call, middle}, {return_to, top}],
                 [middle,bottom] => [{call, middle}, {call, bottom}, {return_to,middle}, {return_to,top}]
                }},

              {[{'catch',MiddleCall}, {tail,BottomCall}, exception],
               #{[bottom] => [{call, bottom}, {return_to, top}],
                 [middle] => [{call, middle}, {return_to, top}],
                 [middle,bottom] => [{call, middle}, {call, bottom}, {return_to,top}]
                }},

              {[{'catch',MiddleCall},  {body,BottomCall}, return],
               #{[bottom] => [{call, bottom}, {return_to, middle}],
                 [middle] => [{call, middle}, {return_to, top}],
                 [middle,bottom] => [{call, middle}, {call, bottom}, {return_to,middle}, {return_to,top}]
                }},

              {[{'catch',MiddleCall}, {'catch',BottomCall}, return],
               #{[bottom] => [{call,bottom}, {return_to,middle}],
                 [middle] => [{call, middle}, {return_to, top}],
                 [middle,bottom] => [{call, middle}, {call, bottom}, {return_to,middle}, {return_to,top}]
                }},

              {[{'catch',MiddleCall}, {tail,BottomCall}, return],
               #{[bottom] => [{call,bottom}, {return_to,top}],
                 [middle] => [{call, middle}, {return_to, top}],
                 [middle,bottom] => [{call, middle}, {call, bottom}, {return_to,top}]
                }}
             ],

    [begin
         %% Make the call sequence
         io_format("CallSequence = ~p\n", [CallSequence]),
         top(CallSequence),

         %% Construct expected trace messages
         Exp = [[{Tracer, {trace, Tracee, CallOrReturnTo, {?MODULE, Func, 1}}}
                 || {CallOrReturnTo, Func} <- maps:get(TracedFuncs, TraceMap, [])]
                || {_Session, Tracer, TracedFuncs} <- Traced],


         io_format("Exp = ~p\n", [Exp]),
         receive_parallel_list(Exp)
     end
     || {CallSequence, TraceMap} <- Script],
    ok.

top([{'catch',local} | T]) ->
    display("top(catch local)"),
    [(catch middle(T)) | 1];
top([{'catch',extern} | T]) ->
    display("top(catch extern)"),
    [(catch ?MODULE:middle(T)) | 1].

middle([{body,local} | T]) ->
    display("middle(local)"),
    [bottom(T) | 1];
middle([{body,extern} | T]) ->
    display("middle(extern)"),
    [?MODULE:bottom(T) | 1];
middle([{'catch',local} | T]) ->
    display("middle(catch local)"),
    [(catch bottom(T)) | 1];
middle([{'catch',extern} | T]) ->
    display("middle(catch extern)"),
    [(catch ?MODULE:bottom(T)) | 1];
middle([{tail,local} | T]) ->
    display("middle(tail local)"),
    bottom(T);
middle([{tail,extern} | T]) ->
    display("middle(tail extern)"),
    ?MODULE:bottom(T).

bottom([return | T]) ->
    display("bottom(return)"),
    T;
bottom([exception]) ->
    display("bottom(exception)"),
    error(exception).

display(Term) ->
    case get(display) of
        true ->
            erlang:display(Term);
        _ ->
            true
    end.

io_format(Frmt, List) ->
    case get(display) of
        true ->
            io:format(Frmt, List);
        _ ->
            ok
    end.


system_monitor_info(_Config) ->
    undefined = erlang:system_monitor(),

    S = trace:session_create(system_monitor, self(), []),

    ok = trace:system(S, large_heap, 1_234_567),
    {system, [{large_heap,1_234_567}]} = trace:info(S, system, all),

    ok = trace:system(S, long_gc, 2_345),
    {system, L1} = trace:info(S, system, all),
    [{large_heap,1_234_567}, {long_gc,2_345}] = lists:sort(L1),

    ok = trace:system(S, long_message_queue, {22,33}),
    {system, L2} = trace:info(S, system, all),
    [{large_heap,1_234_567}, {long_gc,2_345}, {long_message_queue,{22,33}}] = lists:sort(L2),

    ok = trace:system(S, long_schedule, 3_456),
    {system, L3} = trace:info(S, system, all),
    [{large_heap,1_234_567}, {long_gc,2_345},
     {long_message_queue,{22,33}},
     {long_schedule,3_456}] = lists:sort(L3),

    ok = trace:system(S, busy_port, true),
    {system, L4} = trace:info(S, system, all),
    [busy_port,
     {large_heap,1_234_567}, {long_gc,2_345},
     {long_message_queue,{22,33}},
     {long_schedule,3_456}] = lists:sort(L4),

    ok = trace:system(S, busy_dist_port, true),
    {system, L5} = trace:info(S, system, all),
    L5s = lists:sort(L5),
    [busy_dist_port,
     busy_port,
     {large_heap,1_234_567},
     {long_gc,2_345},
     {long_message_queue,{22,33}},
     {long_schedule,3_456}] = L5s,

    undefined = erlang:system_monitor(),

    ok = trace:system(S, large_heap, false),
    {system, L6} = trace:info(S, system, all),
    L6exp = lists:keydelete(large_heap, 1, L5s),
    L6exp = lists:sort(L6),

    ok = trace:system(S, long_message_queue, false),
    {system, L7} = trace:info(S, system, all),
    L7exp = lists:keydelete(long_message_queue, 1, L6exp),
    L7exp = lists:sort(L7),

    ok = trace:system(S, busy_port, false),
    {system, L8} = trace:info(S, system, all),
    L8exp = lists:delete(busy_port, L7exp),
    L8exp = lists:sort(L8),

    ok = trace:system(S, long_schedule, false),
    {system, L9} = trace:info(S, system, all),
    L9exp = lists:keydelete(long_schedule, 1, L8exp),
    L9exp = lists:sort(L9),

    ok = trace:system(S, busy_dist_port, false),
    {system, L10} = trace:info(S, system, all),
    L10exp = lists:delete(busy_dist_port, L9exp),
    L10exp = lists:sort(L10),

    ok = trace:system(S, long_gc, false),
    {system, []} = trace:info(S, system, all),

    undefined = erlang:system_monitor(),
    ok.


system_monitor_long_msgq(_Config) ->
    Tester = self(),
    Receiver = spawn_link(fun () -> message_receiver() end),

    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S1 = trace:session_create(system_monitor_long_msgq, Tracer1, []),
    S2 = trace:session_create(system_monitor_long_msgq, Tracer2, []),

    sysmon_long_msgq(S1, Tracer1, S2, Tracer2, Receiver),
    sysmon_long_msgq(S2, Tracer2, S1, Tracer1, Receiver),

    trace:session_destroy(S1),
    trace:session_destroy(S2),

    unlink(Receiver),
    exit(Receiver, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),
    ok.

sysmon_long_msgq(S1, Tracer1, S2, Tracer2, Receiver) ->
    trace:system(S1, long_message_queue, {50,70}),
    trace:system(S2, long_message_queue, {60,80}),

    [Receiver ! message || _ <- lists:seq(1,50)],   % 50
    receive_nothing(),

    [begin
         [begin
              [Receiver ! message || _ <- lists:seq(1,10)],   % 60
              receive_nothing(),

              [Receiver ! message || _ <- lists:seq(1,10)],   % 70
              {Tracer1, {monitor,Receiver,long_message_queue,true}} = receive_any(),
              receive_nothing(),

              [Receiver ! message || _ <- lists:seq(1,10)],   % 80
              {Tracer2, {monitor,Receiver,long_message_queue,true}} = receive_any(),
              receive_nothing(),

              message_receive_order(Receiver, 10),            % 70
              receive_nothing(),

              [Receiver ! message || _ <- lists:seq(1,10)],   % 80
              receive_nothing(),

              message_receive_order(Receiver, 20),            % 60
              {Tracer2, {monitor,Receiver,long_message_queue,false}} = receive_any(),
              receive_nothing(),

              [Receiver ! message || _ <- lists:seq(1,10)],   % 70
              receive_nothing(),

              message_receive_order(Receiver, 20),            % 50
              {Tracer1, {monitor,Receiver,long_message_queue,false}} = receive_any(),
              receive_nothing()
          end
          || _ <- [1,2]
         ],

         trace:system(S1, long_message_queue, false),

         [Receiver ! message || _ <- lists:seq(1,20)],   % 70
         receive_nothing(),

         [Receiver ! message || _ <- lists:seq(1,10)],   % 80
         {Tracer2, {monitor,Receiver,long_message_queue,true}} = receive_any(),
         receive_nothing(),

         message_receive_order(Receiver, 10),            % 70
         receive_nothing(),

         message_receive_order(Receiver, 10),            % 60
         {Tracer2, {monitor,Receiver,long_message_queue,false}} = receive_any(),
         receive_nothing(),

         message_receive_order(Receiver, 10),            % 50
         receive_nothing(),

         trace:system(S1, long_message_queue, {50,70})
     end
     || _ <- [1,2]],

    %% Set same limits as S2
    %% and test that we can produce more than one message at a time
    trace:system(S1, long_message_queue, {60,80}),

    [Receiver ! message || _ <- lists:seq(1,29)],   % 79
    receive_nothing(),

    [Receiver ! message || _ <- lists:seq(1,1)],   % 80
    receive_parallel(
      {[{Tracer1, {monitor,Receiver,long_message_queue,true}}],
       [{Tracer2, {monitor,Receiver,long_message_queue,true}}]}),
    receive_nothing(),

    message_receive_order(Receiver, 19),            % 61
    receive_nothing(),

    message_receive_order(Receiver, 1),             % 60
    receive_parallel(
      {[{Tracer1, {monitor,Receiver,long_message_queue,false}}],
       [{Tracer2, {monitor,Receiver,long_message_queue,false}}]}),
    receive_nothing(),

    message_receive_order(Receiver, 60),            % 0
    receive_nothing(),

    ok.

message_receiver() ->
    receive
        {'receive', N, From} ->
            [receive_any() || _ <- lists:seq(1,N)],
            From ! {done, N, self()}
    end,
    message_receiver().

message_receive_order(Receiver, N) ->
    Receiver ! {'receive', N, self()},
    receive
        {done, N, Receiver} -> ok
    end.


destroy(_Config) ->
    Name = ?MODULE,
    {_,SName1}=S1 = trace:session_create(Name, self(), []),

    %% Destroy session with trace_session_destroy
    destroy_do(SName1, fun() -> trace:session_destroy(S1) end),

    {_,SName2}=S2 = trace:session_create(Name, self(), []),

    %% Destroy session with GC (magic bin destructor)
    put(session, S2),
    destroy_do(SName2, fun() -> erase(session),
                               erlang:garbage_collect(),
                               wait_bp_finish(),
                               ok
                      end),

    ok.

destroy_do(SName, Destroyer) ->
    Exp = {?MODULE, exported, 1},
    Loc = {?MODULE, local, 1},
    CallMS = [{[arg],[],[]}],
    SendMS = [{[self(), message],[],[]}],
    RecvMS = [{[node(), self(), message],[],[]}],
    [Port|_] = erlang:ports(),

    1 = trace:function(SName, Exp, CallMS, [global]),
    1 = trace:function(SName, Loc, CallMS, [local]),
    0 = trace:function(SName, on_load, CallMS, [local]),
    1 = trace:send(SName, SendMS, []),
    1 = trace:recv(SName, RecvMS, []),
    0 = trace:process(SName, new_processes, true, [all]),
    0 = trace:port(SName, new_ports, true, [all]),
    1 = trace:process(SName, self(), true, [procs]),
    1 = trace:port(SName, Port, true, [ports]),

    [SName] = trace:session_info(Exp),
    [SName] = trace:session_info(Loc),
    [SName] = trace:session_info(on_load),
    SendSessions1 = trace:session_info(send),
    true = lists:member(SName, SendSessions1),
    RecvSessions1 = trace:session_info('receive'),
    true = lists:member(SName, RecvSessions1),
    [SName] = trace:session_info(new_processes),
    [SName] = trace:session_info(new_ports),
    [SName] = trace:session_info(self()),
    [SName] = trace:session_info(Port),

    {traced, global} = trace:info(SName, Exp, traced),
    {traced, local} = trace:info(SName, Loc, traced),
    {traced, local} = trace:info(SName, on_load, traced),
    {match_spec, SendMS} = trace:info(SName, send, match_spec),
    {match_spec, RecvMS} = trace:info(SName, 'receive', match_spec),
    {flags, [_|_]} = trace:info(SName, new_processes, flags),
    {flags, [_|_]} = trace:info(SName, new_ports, flags),
    {flags, [procs]} = trace:info(SName, self(), flags),
    {flags, [ports]} = trace:info(SName, Port, flags),

    Destroyer(),

    [] = trace:session_info(Exp),
    [] = trace:session_info(Loc),
    [] = trace:session_info(on_load),
    SendSessions2 = trace:session_info(send),
    false = lists:member(SName, SendSessions2),
    RecvSessions2 = trace:session_info('receive'),
    false = lists:member(SName, RecvSessions2),
    [] = trace:session_info(new_processes),
    [] = trace:session_info(new_ports),
    [] = trace:session_info(self()),
    [] = trace:session_info(Port),

    {'EXIT',{badarg,_}} = (catch trace:info(SName, Exp, traced)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, Loc, traced)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, on_load, traced)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, send, match_spec)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, 'receive', match_spec)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, new_processes, flags)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, new_ports, flags)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, self(), flags)),
    {'EXIT',{badarg,_}} = (catch trace:info(SName, Port, flags)),
    true.

negative(_Config) ->
    Tracee = spawn_link(fun() -> receive done -> ok end end),
    SessionTracer = spawn_link(fun() -> receive done -> ok end end),
    OtherTracer = spawn_link(fun() -> receive done -> ok end end),
    MFA = {?MODULE, foo, 0},
    S = trace:session_create(?MODULE, SessionTracer, []),

    %% Specified tracer not allowed
    {'EXIT',{badarg,_}} = (catch trace:process(S, Tracee, true, [call, {tracer,OtherTracer}])),
    1 = catch trace:process(S, Tracee, true, [call]),
    1 = catch trace:process(S, Tracee, false, [call]),

    %% Specified meta tracer not allowed
    {'EXIT',{badarg,_}} = (catch trace:function(S, MFA, true, [{meta,OtherTracer}])),
    {'EXIT',{badarg,_}} = (catch trace:function(S, MFA, true, [{meta,erl_tracer,OtherTracer}])),
    1 = trace:function(S, MFA, true, [meta]),
    1 = trace:function(S, MFA, false, [meta]),

    FuncInfoItems = [all, traced, match_spec, meta, meta_match_spec,
                     call_count, call_time, call_memory],
    [{Item,undefined} = trace:info(S, {?MODULE, false, 77}, Item)
     || Item <- FuncInfoItems],
    [{Item,false} = trace:info(S, MFA, Item)
     || Item <- FuncInfoItems],

    trace:session_destroy(S),
    ok.

%% Test error reporting of module 'trace'.
error_info(_Config) ->
    %% Pick up external pid and port.
    {ok, Peer, ExternalNode} = ?CT_PEER(),
    ExternalPid = rpc:call(ExternalNode, erlang, whereis, [code_server]),
    ExternalPort = hd(rpc:call(ExternalNode, erlang, ports, [])),
    Tracer = spawn(fun() -> receive never -> false end end),
    TraceSession = trace:session_create(?MODULE, Tracer, []),

    L = [
         {process, [TraceSession, a, true, []]},
         {process, [TraceSession, ExternalPid, true, []]},
         {process, [TraceSession, self(), not_boolean, []]},
         {process, [TraceSession, self(), true, bad_flags]},
         {process, [TraceSession, self(), true, [bad_flag]]},
         {process, [TraceSession, self(), true, [call|send]]},

         {port, [TraceSession, a, true, []]},
         {port, [TraceSession, ExternalPort, true, []]},
         {port, [TraceSession, self(), not_boolean, []]},
         {port, [TraceSession, self(), true, bad_flags]},
         {port, [TraceSession, self(), true, [bad_flag]]},
         {port, [TraceSession, self(), true, [send|'receive']]},


         {function, [TraceSession, a, true, []]},
         {function, [TraceSession, {?MODULE,'_','_'}, not_boolean, []]},
         {function, [TraceSession, {?MODULE,'_','_'}, true, bad_flags]},
         {function, [TraceSession, {?MODULE,'_','_'}, true, [bad_flag]]},
         {function, [TraceSession, {?MODULE,'_','_'}, true, [local|meta]]},
         {function, [TraceSession, {?MODULE,'_','_'}, [{[self(), '_'],[],[]}], [call_count]]},

         {send, [TraceSession, a, []]},
         {send, [TraceSession, true, bad_flags]},
         {send, [TraceSession, true, [bad_flag]]},

         {recv, [TraceSession, a, []]},
         {recv, [TraceSession, true, bad_flags]},
         {recv, [TraceSession, true, [bad_flag]]},

         {info, [bad_session, self(), flags]},
         {info, [make_ref(), self(), flags]},
         {info, [atomics:new(1,[]), self(), flags]},
         {info, [TraceSession, ExternalPid, flags]},
         {info, [TraceSession, self(), bad_item]},

         {session_create, ["bad name", self(), []]},
         {session_create, [name, bad_tracer, []]},
         {session_create, [name, self(), bad_option]},
         {session_create, [name, self(), [bad_option]]},

         {session_destroy, [bad_session]},
         {session_destroy, [make_ref()]},
         {session_destroy, [atomics:new(1,[])]},

         {session_info, [ExternalPid]},

         {delivered, 2} %% Cannot fail
        ],

    try
        error_info_lib:test_error_info(trace, L, [allow_nyi])
    after
        peer:stop(Peer)
    end.


%% Some basic testing of call_time and call_memory
timem_basic(_Config) ->
    Tracer = spawn(fun F() -> receive M -> io:format("~p~n",[M]), F() end end),
    Session = trace:session_create(my_session, Tracer, []),

    Pid = self(),
    1 = trace:process(Session, Pid, true, [call]),
    1 = trace:function(Session, {lists,seq,2}, [], [call_time]),
    {call_time, []} =  trace:info(Session, {lists,seq,2}, call_time),
    {call_memory, false} = trace:info(Session, {lists,seq,2}, call_memory),


    lists:seq(1,10),
    {call_time, [{Pid, 1, 0, T1}]}=CT1 =  trace:info(Session, {lists,seq,2}, call_time),
    {call_memory, false}=CMF =  trace:info(Session, {lists,seq,2}, call_memory),
    CT1 =  trace:info(Session, {lists,seq,2}, call_time),
    CMF =  trace:info(Session, {lists,seq,2}, call_memory),

    lists:seq(1,10),
    {call_time, [{Pid, 2, 0, T2}]}=CT2 = trace:info(Session, {lists,seq,2}, call_time),
    true = (T2 >= T1),
    CMF = trace:info(Session, {lists,seq,2}, call_memory),
    CT2 = trace:info(Session, {lists,seq,2}, call_time),

    1 = trace:function(Session, {lists,seq,2}, [], [call_memory]),
    CT2 = trace:info(Session, {lists,seq,2}, call_time),
    {call_memory, []} = trace:info(Session, {lists,seq,2}, call_memory),

    lists:seq(1,10),
    {call_time, [{Pid, 3, 0, T3}]}=CT3 = trace:info(Session, {lists,seq,2}, call_time),
    true = (T3 >= T2),
    {call_memory, [{Pid, 1, M1}]}=CM1 = trace:info(Session, {lists,seq,2}, call_memory),
    CT3 = trace:info(Session, {lists,seq,2}, call_time),
    CM1 = trace:info(Session, {lists,seq,2}, call_memory),

    lists:seq(1,10),
    {call_time, [{Pid, 4, 0, T4}]}=CT4 = trace:info(Session, {lists,seq,2}, call_time),
    true = (T4 >= T3),
    {call_memory, [{Pid, 2, M2}]}=CM2 = trace:info(Session, {lists,seq,2}, call_memory),
    true = (M2 > M1),
    CT4 = trace:info(Session, {lists,seq,2}, call_time),
    CM2 = trace:info(Session, {lists,seq,2}, call_memory),

    %% Turn off call_time
    1 = trace:function(Session, {lists,seq,2}, false, [call_time]),
    {call_time, false} = trace:info(Session, {lists,seq,2}, call_time),
    CM2 = trace:info(Session, {lists,seq,2}, call_memory),

    lists:seq(1,10),
    {call_time, false} = trace:info(Session, {lists,seq,2}, call_time),
    {call_memory, [{Pid, 3, M3}]} = trace:info(Session, {lists,seq,2}, call_memory),
    true = (M3 > M2),

    true = trace:session_destroy(Session),
    ok.

wait_bp_finish() ->
    wait_thread_progress(5).

wait_thread_progress(0) -> ok;
wait_thread_progress(N) ->
    ok = erts_debug:set_internal_state(wait, thread_progress),
    wait_thread_progress(N-1).

foo() ->
    ok.

exported(X) ->
    local(X).

local(_) ->
    ok.

tracer(Name, Tester) ->
    tracer(Name, Tester, true).

tracer(Name, Tester, Display) ->
    case Display of
        true -> put(display,true);
        _ -> ok
    end,
    tracer_loop(Name, Tester).


tracer_loop(Name, Tester) ->
    receive M ->
            io_format("~p ~p got message: ~p\n", [Name, self(), M]),
            Tester ! {self(), M}
    end,
    tracer_loop(Name, Tester).


%% OTP-19840: Verify setting/clearing of 'is_bif_traced' in export entry
%% works correctly for multiple sessions.
is_bif_traced(_Config) ->
    CallTypes = [global, local],
    [is_bif_traced_do(CT1, CT2, CT3)
     || CT1 <- CallTypes, CT2 <- CallTypes, CT3 <- CallTypes],
    ok.

is_bif_traced_do(CT1, CT2, CT3) ->
    io:format("CT1=~w, CT2=~w, CT3=~w\n", [CT1, CT2, CT3]),

    Tester = self(),
    TracerFun = fun F() -> receive M -> Tester ! {self(), M} end, F() end,
    T1 = spawn_link(TracerFun),
    S1 = trace:session_create(one, T1, []),

    %% A benign BIF call that does not get optimized away
    BIF = {erlang,phash2,1},
    {M,F,A} = BIF,
    true = erlang:is_builtin(M,F,A),

    trace:function(S1, BIF, true, [CT1]),
    trace:process(S1, self(), true, [call]),

    M:F("S1"),
    {T1, {trace,Tester,call,{M,F,["S1"]}}} = receive_any(),

    T2 = spawn_link(TracerFun),
    S2 = trace:session_create(two, T2, []),
    trace:function(S2, BIF, true, [CT2]),
    trace:process(S2, self(), true, [call]),

    M:F("S1 & S2"),
    receive_parallel_list(
      [[{T1, {trace,Tester,call,{M,F,["S1 & S2"]}}}],
       [{T2, {trace,Tester,call,{M,F,["S1 & S2"]}}}]]),

    T3 = spawn_link(TracerFun),
    S3 = trace:session_create(three, T3, []),
    trace:function(S3, BIF, true, [CT3]),
    trace:process(S3, self(), true, [call]),

    M:F("S1 & S2 & S3"),
    receive_parallel_list(
      [[{T1, {trace,Tester,call,{M,F,["S1 & S2 & S3"]}}}],
       [{T2, {trace,Tester,call,{M,F,["S1 & S2 & S3"]}}}],
       [{T3, {trace,Tester,call,{M,F,["S1 & S2 & S3"]}}}]]),

    %% Remove not last BIF trace nicely
    trace:function(S1, BIF, false, [CT1]),
    M:F("S2 & S3"),
    receive_parallel_list(
      [[{T2, {trace,Tester,call,{M,F,["S2 & S3"]}}}],
       [{T3, {trace,Tester,call,{M,F,["S2 & S3"]}}}]]),

    %% Remove not last BIF trace by session destruction
    trace:session_destroy(S2),
    M:F("S3"),
    receive_parallel_list(
      [[{T3, {trace,Tester,call,{M,F,["S3"]}}}]]),

    %% Remove last BIF trace nicely
    trace:function(S3, BIF, false, [CT3]),
    M:F("no trace"),
    receive_nothing(),

    trace:function(S1, BIF, true, [CT1]),
    M:F("S1"),
    receive_parallel_list(
      [[{T1, {trace,Tester,call,{M,F,["S1"]}}}]]),

    %% Remove last BIF trace by session destruction
    trace:session_destroy(S1),
    M:F("no trace"),
    receive_nothing(),

    trace:session_destroy(S3),
    ok.


receive_any() ->
    receive_any(1000).

receive_any(Timeout) ->
    receive M -> M
    after Timeout -> timeout
    end.

receive_nothing() ->
    timeout = receive_any(10).

%% Argument is a tuple of lists with expected messages to receive.
%% Each list is internally ordered according to expected reception.
%% The different lists in the tuple are mutually unordered.
%% '_' can be used as wildcard in message terms.
receive_parallel({}) ->
    ok;
receive_parallel(Tuple) ->
    M = receive_any(),
    receive_parallel(M, Tuple, tuple_size(Tuple)).

receive_parallel(M, Tuple, I) when I > 0 ->
    [Pattern|Tail] = element(I, Tuple),
    case {match(Pattern, M), Tail} of
        {true, []} ->
            receive_parallel(erlang:delete_element(I, Tuple));
        {true, _} ->
            receive_parallel(setelement(I, Tuple, Tail));
        {false, _} ->
            receive_parallel(M, Tuple, I-1)
    end;
receive_parallel(M, Tuple, 0) ->
    Failed = [H || [H|_] <- tuple_to_list(Tuple)],
    io:format("Expected any of:\n~p\n", [Failed]),
    io:format("Got message:\n~p\n", [M]),
    ct:fail("Unexpected messages: ~p", [M]).

%% Same as receive_parallel/1 but accepts a *list* of message lists
%% and the message lists are allowed to be empty meaning no expected messages.
receive_parallel_list(List0) ->
    List1 = lists:filter(fun(E) -> E =/= [] end, List0),
    receive_parallel(list_to_tuple(List1)).

receive_unsorted(Expect) ->
    receive_unsorted(Expect, length(Expect)).

receive_unsorted(Expect0, Cnt) when Cnt > 0 ->
    M = receive_any(),
    Expect1 = match_and_delete(M, Expect0, []),
    receive_unsorted(Expect1, Cnt-1);
receive_unsorted(_, 0) ->
    ok.

match_and_delete(M, [Pattern | Tail], Failed) ->
    case match(Pattern, M) of
        true -> Failed ++ Tail;
        false ->
            match_and_delete(M, Tail, [Pattern | Failed])
    end;
match_and_delete(M, [], Failed) ->
    io:format("Expected any of:\n~p\n", [Failed]),
    io:format("Got message:\n~p\n", [M]),
    ct:fail("Unexpected messages: ~p", [M]).



match(A, A) -> true;
match('_', _) -> true;
match([Ah|At], [Bh|Bt]) ->
    match(Ah, Bh) andalso match(At, Bt);
match(A, B) when tuple_size(A) =:= tuple_size(B) ->
    match(tuple_to_list(A), tuple_to_list(B));
match(_A, _B) ->
    false.


end_of_list(_Config) ->
    ok.
