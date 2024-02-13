%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
         destroy/1,
         end_of_list/1]).

-include_lib("common_test/include/ct.hrl").


-undef(line).
-ifdef(debug).
-define(line,io:format("line ~p\n",[?LINE]),erlang:display(?LINE)).
-else.
-define(line,void).
-endif.

-export([foo/0, exported/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [
     basic,
     call,
     meta,
     on_load,
     trace_info_on_load,
     procs,
     test_set_on_spawn,
     test_set_on_first_spawn,
     test_set_on_link,
     test_set_on_first_link,
     destroy,
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

erlang_trace(legacy, PidPortSpec, How, FlagList) ->
    erlang:trace(PidPortSpec, How, FlagList);
erlang_trace(Session, PidPortSpec, How, FlagList) ->
    erlang:trace(Session, PidPortSpec, How, FlagList).

erlang_trace_pattern(legacy, MFA, MS, FlagList) ->
    erlang:trace_pattern(MFA, MS, FlagList);
erlang_trace_pattern(Session, MFA, MS, FlagList) ->
    erlang:trace_pattern(Session, MFA, MS, FlagList).


on_load(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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
    S = erlang:trace_session_create(?MODULE, Tracer, []),
    1 = erlang:trace(S, self(), true, [call]),
    0 = erlang:trace_pattern(S, on_load, true, []),
    {all,[{traced,global},
      {match_spec,[]},
      {meta,false},
      {meta_match_spec,false},
      {call_count,false}]} = erlang:trace_info(S, on_load, all),
    ok.

on_load1(Session, Tracer) ->
    Tracee = self(),
    1 = erlang:trace(Session, self(), true, [call]),
    0 = erlang:trace_pattern(Session, on_load, true, []),
    dummy:module_info(),
    erlang:delete_module(dummy),
    erlang:purge_module(dummy),
    ?line,
    {Tracer, {trace,Tracee,call,{dummy,module_info,[]}}} = receive_any(),
    1 = erlang:trace(Session, self(), false, [call]),
    dummy:module_info(),
    erlang:delete_module(dummy),
    erlang:purge_module(dummy),
    ?line,
    timeout = receive_nothing(),
    0 = erlang:trace_pattern(Session, on_load, false, []),
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
    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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

    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

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
    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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

    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

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
    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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
    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

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
    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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
    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),
    ok.
set_on_spawn(Session, Tracer) ->
    1 = erlang:trace(Session, self(), true, [procs, set_on_spawn]),
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
    1 = erlang:trace(Session, self(), false, [procs, set_on_spawn]),
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
        {Tracer2,{trace, '_', exit, die}}]
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
    1 = erlang:trace(Session, self(), true, [procs, set_on_first_spawn]),
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

    1 = erlang:trace(Session, self(), false, [procs, set_on_first_spawn]),
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
    1 = erlang:trace(Session, self(), true, [procs, set_on_link]),
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
    1 = erlang:trace(Session, self(), false, [procs, set_on_link]),
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
    1 = erlang:trace(Session, self(), true, [procs, set_on_link]),
    Child4 = spawn(fun() -> receive M -> M end end),
    link(Child4),
    receive_parallel(
      {[{Tracer,{trace, Tracee, spawn,Child4,'_'}},
        {Tracer,{trace, Tracee, link, Child4}}],

       [{Tracer,{trace, Child4, getting_linked, Tracee}}]
      }),
    1 = erlang:trace(Session, Child4, false, [procs, set_on_link]),
    unlink(Child4),
    exit(Child4, die),
    {Tracer,{trace, Tracee, unlink, Child4}} = receive_any(),

    1 = erlang:trace(Session, self(), false, [procs, set_on_link]),
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
    1 = erlang:trace(Session, self(), true, [procs, set_on_first_link]),
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
    1 = erlang:trace(Session, Tracee, true, [set_on_first_link]),
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
    1 = erlang:trace(Session, Tracee, true, [set_on_first_link]),
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
    1 = erlang:trace(Session, self(), false, [procs, set_on_first_link]),
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

    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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

    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

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

    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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

    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

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

    receive_unsorted(
      [{Tracer1, {trace, Tracee, register, RegName}},
       {Tracer2, {trace, Tracee, register, RegName}}]),
    receive_unsorted(
      [{Tracer1, {trace, Tracee, unregister, RegName}},
       {Tracer2, {trace, Tracee, unregister, RegName}}]),

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

    S0 = erlang:trace_session_create(session0, undefined, []),
    S1 = erlang:trace_session_create(session1, Tracer1, []),
    S2 = erlang:trace_session_create(session2, Tracer2, []),

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

    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

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

    MFArity = {?MODULE,foo,0},
    MFArgs = {?MODULE,foo,[]},
    1 = erlang:trace_pattern(MFArity,true,[{meta,Tracer0}]),

    ?line,
    foo(),
    {Tracer0, {trace_ts,P,call,MFArgs,{_,_,_}}} = receive_any(),

    ?line,
    ?MODULE:foo(),
    {Tracer0, {trace_ts,P,call,MFArgs,{_,_,_}}} = receive_any(),

    1 = erlang:trace_pattern(MFArity,false,[meta]),

    ?line,
    foo(),
    timeout = receive_nothing(),

    unlink(Tracer0),
    exit(Tracer0, die),

    ok.

destroy(_Config) ->
    SName = ?MODULE,
    S1 = erlang:trace_session_create(SName, self(), []),

    %% Destroy session with trace_session_destroy
    destroy_do(SName, fun() -> erlang:trace_session_destroy(S1) end),

    S2 = erlang:trace_session_create(SName, self(), []),

    %% Destroy session with GC (magic bin destructor)
    put(session, S2),
    destroy_do(SName, fun() -> erase(session),
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

    1 = erlang:trace_pattern(SName, Exp, CallMS, [global]),
    1 = erlang:trace_pattern(SName, Loc, CallMS, [local]),
    0 = erlang:trace_pattern(SName, on_load, CallMS, [local]),
    1 = erlang:trace_pattern(SName, send, SendMS, []),
    1 = erlang:trace_pattern(SName, 'receive', RecvMS, []),
    0 = erlang:trace(SName, new_processes, true, [all]),
    0 = erlang:trace(SName, new_ports, true, [all]),
    1 = erlang:trace(SName, self(), true, [procs]),
    1 = erlang:trace(SName, Port, true, [ports]),

    [SName] = erlang:trace_session_info(Exp),
    [SName] = erlang:trace_session_info(Loc),
    [SName] = erlang:trace_session_info(on_load),
    SendSessions1 = erlang:trace_session_info(send),
    true = lists:member(SName, SendSessions1),
    RecvSessions1 = erlang:trace_session_info('receive'),
    true = lists:member(SName, RecvSessions1),
    [SName] = erlang:trace_session_info(new_processes),
    [SName] = erlang:trace_session_info(new_ports),
    [SName] = erlang:trace_session_info(self()),
    [SName] = erlang:trace_session_info(Port),

    {traced, global} = erlang:trace_info(SName, Exp, traced),
    {traced, local} = erlang:trace_info(SName, Loc, traced),
    {traced, local} = erlang:trace_info(SName, on_load, traced),
    {match_spec, SendMS} = erlang:trace_info(SName, send, match_spec),
    {match_spec, RecvMS} = erlang:trace_info(SName, 'receive', match_spec),
    {flags, [_|_]} = erlang:trace_info(SName, new_processes, flags),
    {flags, [_|_]} = erlang:trace_info(SName, new_ports, flags),
    {flags, [procs]} = erlang:trace_info(SName, self(), flags),
    {flags, [ports]} = erlang:trace_info(SName, Port, flags),

    Destroyer(),

    [] = erlang:trace_session_info(Exp),
    [] = erlang:trace_session_info(Loc),
    [] = erlang:trace_session_info(on_load),
    SendSessions2 = erlang:trace_session_info(send),
    false = lists:member(SName, SendSessions2),
    RecvSessions2 = erlang:trace_session_info('receive'),
    false = lists:member(SName, RecvSessions2),
    [] = erlang:trace_session_info(new_processes),
    [] = erlang:trace_session_info(new_ports),
    [] = erlang:trace_session_info(self()),
    [] = erlang:trace_session_info(Port),

    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, Exp, traced)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, Loc, traced)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, on_load, traced)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, send, match_spec)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, 'receive', match_spec)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, new_processes, flags)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, new_ports, flags)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, self(), flags)),
    {'EXIT',{badarg,_}} = (catch erlang:trace_info(SName, Port, flags)),
    true.

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
    receive M ->
            io:format("~p ~p got message: ~p\n", [Name, self(), M]),
            Tester ! {self(), M}
    end,
    tracer(Name, Tester).


receive_any() ->
    receive_any(1000).

receive_any(Timeout) ->
    receive M -> M
    after Timeout -> timeout
    end.

receive_nothing() ->
    receive_any(10).

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
