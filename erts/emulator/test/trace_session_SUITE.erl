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

-export([all/0, suite/0]).

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
         end_of_list/1]).

-include_lib("common_test/include/ct.hrl").


-undef(line).
-ifdef(debug).
-define(line,io:format("line ~p\n",[?LINE]),erlang:display(?LINE)).
-else.
-define(line,void).
-endif.

-export([foo/0]).

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
     end_of_list].
on_load(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),
    on_load1(Tracer1, [{session, S1}]),
    on_load2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    on_load2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    on_load2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    on_load2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    on_load2(Tracer0, [{tracer,Tracer0},{session,S0}],
            Tracer1, [{session,S1}]),

    ok.
trace_info_on_load(_Config) ->
    Tester = self(),
    Tracer = spawn_link(fun() -> tracer("Tracer", Tester) end),
    S = erlang:trace_session_create([{tracer,Tracer}]),
    1 = erlang:trace(self(), true, [call,{session,S}]),
    0 = erlang:trace_pattern(on_load, true, [{session,S}]),
    {all,[{traced,global},
      {match_spec,[]},
      {meta,false},
      {meta_match_spec,false},
      {call_count,false}]} = erlang:trace_info(S, on_load, all),
    ok.
on_load1(Tracer, Opts) ->
    Tracee = self(),
    1 = erlang:trace(self(), true, [call|Opts]),
    Opts1 = proplists:delete(tracer, Opts),
    0 = erlang:trace_pattern(on_load, true, Opts1),
    ssh_agent:module_info(),
    erlang:delete_module(ssh_agent),
    erlang:purge_module(ssh_agent),
    ?line,
    {Tracer, {trace,Tracee,call,{ssh_agent,module_info,[]}}} = receive_any(),
    1 = erlang:trace(self(), false, [call|Opts]),
    ssh_agent:module_info(),
    erlang:delete_module(ssh_agent),
    erlang:purge_module(ssh_agent),
    ?line,
    timeout =  receive_any(),
    0 = erlang:trace_pattern(on_load, false, Opts1),
    ok.
on_load2(Tracer1, Opts1, Tracer2, Opts2) ->
    Tracee = self(),
    1 = erlang:trace(self(), true, [call|Opts1]),
    1 = erlang:trace(self(), true, [call|Opts2]),
    Opts1_1 = proplists:delete(tracer, Opts1),
    Opts2_1 = proplists:delete(tracer, Opts2),
    0 = erlang:trace_pattern(on_load, true, Opts1_1),
    0 = erlang:trace_pattern(on_load, true, Opts2_1),
    ssh_agent:module_info(),
    erlang:delete_module(ssh_agent),
    erlang:purge_module(ssh_agent),
    timer:sleep(100),
    ?line,
    receive_unsorted(
        [{Tracer1,{trace,Tracee,call,{ssh_agent,module_info,[]}}},
         {Tracer2,{trace,Tracee,call,{ssh_agent,module_info,[]}}}]),
    1 = erlang:trace(self(), false, [call|Opts1]),
    ssh_agent:module_info(),
    erlang:delete_module(ssh_agent),
    erlang:purge_module(ssh_agent),
    ?line,
    {Tracer2, {trace,Tracee,call,{ssh_agent,module_info,[]}}} = receive_any(),
    1 = erlang:trace(self(), false, [call|Opts2]),
    ssh_agent:module_info(),
    erlang:delete_module(ssh_agent),
    erlang:purge_module(ssh_agent),
    ?line,
    timeout =  receive_any(),

    0 = erlang:trace_pattern(on_load, false, Opts1_1),
    0 = erlang:trace_pattern(on_load, false, Opts2_1),
    ok.

test_set_on_spawn(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_spawn(Tracer1, [{session, S1}]),
    set_on_spawn2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_spawn2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_spawn2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_spawn2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_spawn2(Tracer0, [{tracer,Tracer0},{session,S0}],
            Tracer1, [{session,S1}]),

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
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_first_spawn(Tracer1, [{session, S1}]),
    set_on_first_spawn2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_first_spawn2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_first_spawn2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_first_spawn2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_first_spawn2(Tracer0, [{tracer,Tracer0},{session,S0}],
            Tracer1, [{session,S1}]),

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
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_link(Tracer1, [{session, S1}]),
    set_on_link2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_link2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_link2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_link2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_link2(Tracer0, [{tracer,Tracer0},{session,S0}],
            Tracer1, [{session,S1}]),
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
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_first_link(Tracer1, [{session, S1}]),
    set_on_first_link2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_first_link2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_first_link2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_first_link2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_first_link2(Tracer0, [{tracer,Tracer0},{session,S0}],
            Tracer1, [{session,S1}]),
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
set_on_spawn(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts]),
    Tracee = self(),
    TraceeChild = spawn(fun F() -> receive hej -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), F() end end),
    timer:sleep(100),
    TraceeChild ! hej,
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer,{trace, TraceeChild, spawned,Tracee,'_'}}]),
    receive_unsorted(
        [{Tracer,{trace, TraceeChild, spawn,'_','_'}},
         {Tracer,{trace, '_', spawned,TraceeChild,'_'}},
         {Tracer,{trace, '_', exit, die}},
         {Tracer,{trace, TraceeChild, exit, die}}]),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts]),
    ok.
set_on_spawn2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts2]),
    Tracee = self(),
    TraceeChild = spawn(fun F() -> receive hej -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), F()  end  end),
    timer:sleep(100),
    TraceeChild ! hej,
    timer:sleep(100),
    exit(TraceeChild, die),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer2,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer1,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer2,{trace, TraceeChild, spawned,Tracee,'_'}}]),
    receive_unsorted(
        [{Tracer1,{trace, TraceeChild, spawn,'_','_'}},
         {Tracer2,{trace, TraceeChild, spawn,'_','_'}},
         {Tracer1,{trace, '_', spawned,TraceeChild,'_'}},
         {Tracer2,{trace, '_', spawned,TraceeChild,'_'}},
         {Tracer1,{trace, TraceeChild, exit, die}},
         {Tracer2,{trace, TraceeChild, exit, die}},
         {Tracer1,{trace, '_', exit, die}},
         {Tracer2,{trace, '_', exit, die}}]),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts1]),
    TraceeChild1 = spawn(fun F() -> receive hej -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), F()  end  end),
    timer:sleep(100),
    TraceeChild1 ! hej,
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer2,{trace, Tracee, spawn,TraceeChild1,'_'}},
         {Tracer2,{trace, TraceeChild1, spawned,Tracee,'_'}},
         {Tracer2,{trace, TraceeChild1, spawn,'_','_'}},
         {Tracer2,{trace, '_', spawned,TraceeChild1,'_'}},
         {Tracer2,{trace, TraceeChild1, exit, die}},
         {Tracer2,{trace, '_', exit, die}}]),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts2]),
    ok.
set_on_first_spawn(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_spawn | Opts]),
    Tracee = self(),
    TraceeChild = spawn(fun F() -> receive hej -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), F()  end  end),
    timer:sleep(100),
    TraceeChild ! hej,
    timer:sleep(100),
    exit(TraceeChild, die),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    exit(TraceeChild1, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer,{trace, TraceeChild, spawned,Tracee,'_'}}]),
    receive_unsorted(
        [{Tracer,{trace, TraceeChild, spawn,'_','_'}},
         {Tracer,{trace, Tracee, spawn,TraceeChild1,'_'}},
         {Tracer,{trace, TraceeChild, exit, die}}]),
    1 = erlang:trace(self(), false, [procs, set_on_first_spawn | Opts]),
    ok.
set_on_first_spawn2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_spawn | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_first_spawn | Opts2]),
    Tracee = self(),
    TraceeChild = spawn(fun F() -> receive hej -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), F()  end  end),
    timer:sleep(100),
    TraceeChild ! hej,
    timer:sleep(100),
    exit(TraceeChild, die),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer2,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer1,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer2,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer1,{trace, TraceeChild, spawn,'_','_'}},
         {Tracer2,{trace, TraceeChild, spawn,'_','_'}}]),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, spawn,TraceeChild1,'_'}},
         {Tracer2,{trace, Tracee, spawn,TraceeChild1,'_'}},
         {Tracer1,{trace, TraceeChild, exit, die}},
         {Tracer2,{trace, TraceeChild, exit, die}}]),
    1 = erlang:trace(self(), false, [procs, set_on_first_spawn | Opts1]),
    TraceeChild2 = spawn(fun F() -> receive hej -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), F()  end  end),
    timer:sleep(100),
    TraceeChild2 ! hej,
    timer:sleep(100),
    exit(TraceeChild2, die),
    timer:sleep(100),
    TraceeChild21 = spawn(fun() -> receive M -> M end end),
    exit(TraceeChild21, die),
    timer:sleep(100),

    {Tracer2,{trace, Tracee, spawn,TraceeChild2,_}} = receive_any(),
    {Tracer2,{trace, Tracee, spawn,TraceeChild21,_}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_first_spawn | Opts2]),
    ok.
set_on_link(Tracer, Opts) ->
    %% Test set_on_link via spawn_link/1
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer,{trace, Tracee, link, TraceeChild}},
         {Tracer,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer,{trace, TraceeChild, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer,{trace, Tracee, unlink, TraceeChild}},
         {Tracer,{trace, TraceeChild, getting_unlinked, Tracee}}]),
    {Tracer,{trace, TraceeChild, exit, die}} = receive_any(),
    
    %% Test set_on_link via link/1
    TraceeChild2 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild2),
    timer:sleep(100),
    unlink(TraceeChild2),
    timer:sleep(100),
    exit(TraceeChild2, die),
    timer:sleep(100),
    {Tracer,{trace, Tracee, spawn,TraceeChild2,_}} = receive_any(),
    receive_unsorted(
        [{Tracer,{trace, Tracee, link, TraceeChild2}},
         {Tracer,{trace, TraceeChild2, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer,{trace, Tracee, unlink, TraceeChild2}},
         {Tracer,{trace, TraceeChild2, getting_unlinked, Tracee}}]),
    {Tracer,{trace, TraceeChild2, exit, die}} = receive_any(),
    %% Test that you can disable the tracer but still get traces
    %% from child process
    TraceeChild3 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild3),
    timer:sleep(100),
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts]),
    unlink(TraceeChild3),
    timer:sleep(100),
    exit(TraceeChild3, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild3,'_'}},
         {Tracer,{trace, Tracee, link, TraceeChild3}},
         {Tracer,{trace, TraceeChild3, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer,{trace, TraceeChild3, getting_unlinked, Tracee}},
         {Tracer,{trace, TraceeChild3, exit, die}}]),
    %% Test that you can disable the tracer for the child process
    %% and get messages from the parent
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts]),
    TraceeChild4 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild4),
    timer:sleep(100),
    1 = erlang:trace(TraceeChild4, false, [procs, set_on_link | Opts]),
    unlink(TraceeChild4),
    timer:sleep(100),
    exit(TraceeChild4, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild4,'_'}},
         {Tracer,{trace, Tracee, link, TraceeChild4}},
         {Tracer,{trace, TraceeChild4, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer,{trace, Tracee, unlink, TraceeChild4}}]),
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts]),
    spawn_link(fun() -> receive M -> M end end),
    timeout = receive_any(),
    ok.
set_on_link2(Tracer1, Opts1, Tracer2, Opts2) ->
    %% Test multiple tracers with set_on_link via spawn_link/1
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts2]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer2,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer1,{trace, Tracee, link, TraceeChild}},
         {Tracer2,{trace, Tracee, link, TraceeChild}},
         {Tracer1,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer2,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer1,{trace, TraceeChild, getting_linked, Tracee}},
         {Tracer2,{trace, TraceeChild, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, unlink, TraceeChild}},
         {Tracer2,{trace, Tracee, unlink, TraceeChild}},
         {Tracer1,{trace, TraceeChild, getting_unlinked, Tracee}},
         {Tracer2,{trace, TraceeChild, getting_unlinked, Tracee}}]),
    receive_unsorted(
        [{Tracer1,{trace, TraceeChild, exit, die}},
         {Tracer2,{trace, TraceeChild, exit, die}}]),
    %% Test multiple tracers with set_on_link via link/1
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    ?line,
    link(TraceeChild1),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    receive_unsorted([
        {Tracer1,{trace, Tracee, spawn,TraceeChild1,'_'}},
        {Tracer2,{trace, Tracee, spawn,TraceeChild1,'_'}},
        {Tracer1,{trace, Tracee, link, TraceeChild1}},
        {Tracer2,{trace, Tracee, link, TraceeChild1}},
        {Tracer1,{trace, TraceeChild1, getting_linked, Tracee}},
        {Tracer2,{trace, TraceeChild1, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, unlink, TraceeChild1}},
         {Tracer2,{trace, Tracee, unlink, TraceeChild1}},
         {Tracer1,{trace, TraceeChild1, getting_unlinked, Tracee}},
         {Tracer2,{trace, TraceeChild1, getting_unlinked, Tracee}}]),
    receive_unsorted(
        [{Tracer1,{trace, TraceeChild1, exit, die}},
         {Tracer2,{trace, TraceeChild1, exit, die}}]),
    %% Disable one tracer and test that set_on_link still works for the other tracer
    %% via spawn_link/1
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts1]),
    TraceeChild2 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild2),
    timer:sleep(100),
    exit(TraceeChild2, die),
    receive_unsorted(
        [{Tracer2,{trace, Tracee, spawn,TraceeChild2,'_'}},
         {Tracer2,{trace, Tracee, link, TraceeChild2}},
         {Tracer2,{trace, TraceeChild2, spawned,Tracee,'_'}},
         {Tracer2,{trace, TraceeChild2, getting_linked, Tracee}},
         {Tracer2,{trace, Tracee, unlink, TraceeChild2}},
         {Tracer2,{trace, TraceeChild2, getting_unlinked, Tracee}},
         {Tracer2,{trace, TraceeChild2, exit, die}}]),
    %% Test with link/1
    TraceeChild21 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild21),
    timer:sleep(100),
    unlink(TraceeChild21),
    timer:sleep(100),
    exit(TraceeChild21, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer2,{trace, Tracee, spawn,TraceeChild21,'_'}},
         {Tracer2,{trace, Tracee, link, TraceeChild21}},
         {Tracer2,{trace, TraceeChild21, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer2,{trace, Tracee, unlink, TraceeChild21}},
         {Tracer2,{trace, TraceeChild21, getting_unlinked, Tracee}}]),
    {Tracer2,{trace, TraceeChild21, exit, die}} = receive_any(),
    %% Test that you can disable one of the tracers on the child process
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts1]),
    TraceeChild11 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild11),
    timer:sleep(100),
    1 = erlang:trace(TraceeChild11, false, [procs, set_on_link | Opts1]),
    unlink(TraceeChild11),
    timer:sleep(100),
    exit(TraceeChild11, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, spawn,TraceeChild11,'_'}},
         {Tracer2,{trace, Tracee, spawn,TraceeChild11,'_'}},
         {Tracer1,{trace, Tracee, link, TraceeChild11}},
         {Tracer2,{trace, Tracee, link, TraceeChild11}},
         {Tracer1,{trace, TraceeChild11, getting_linked, Tracee}},
         {Tracer2,{trace, TraceeChild11, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, unlink, TraceeChild11}},
         {Tracer2,{trace, Tracee, unlink, TraceeChild11}},
         {Tracer2,{trace, TraceeChild11, getting_unlinked, Tracee}}]),
    receive_unsorted(
        [{Tracer2,{trace, TraceeChild11, exit, die}}]),
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts1]),
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts2]),
    ok.
set_on_first_link(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_link | Opts]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    TraceeChild1 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer,{trace, Tracee, link, TraceeChild}},
         {Tracer,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer,{trace, TraceeChild, getting_linked, Tracee}}]),
    
    {Tracer,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    {Tracer,{trace, Tracee, link, TraceeChild1}} = receive_any(),
    receive_unsorted(
        [{Tracer,{trace, Tracee, unlink, TraceeChild}},
         {Tracer,{trace, TraceeChild, getting_unlinked, Tracee}}]),
    {Tracer,{trace, TraceeChild, exit, die}} = receive_any(),
    {Tracer,{trace, Tracee, unlink, TraceeChild1}} = receive_any(),
    %% Test that spawn on first link works with link/1
    1 = erlang:trace(Tracee, true, [set_on_first_link | Opts]),
    TraceeChild2 = spawn(fun() -> receive M -> M end end),
    TraceeChild21 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild2),
    link(TraceeChild21),
    timer:sleep(100),
    unlink(TraceeChild2),
    unlink(TraceeChild21),
    timer:sleep(100),
    exit(TraceeChild2, die),
    exit(TraceeChild21,die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild2,'_'}},
         {Tracer,{trace, Tracee, link, TraceeChild2}},
         {Tracer,{trace, Tracee, spawn,TraceeChild21,'_'}},
         {Tracer,{trace, Tracee, link, TraceeChild21}},
         {Tracer,{trace, TraceeChild2, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer,{trace, Tracee, unlink, TraceeChild2}},
         {Tracer,{trace, Tracee, unlink, TraceeChild21}},
         {Tracer,{trace, TraceeChild2, getting_unlinked, Tracee}}]),
    receive_unsorted(
        [{Tracer,{trace, TraceeChild2, exit, die}}]),
    %% Test that spawn on first link flag is not on the child
    1 = erlang:trace(Tracee, true, [set_on_first_link | Opts]),
    TraceeChild3 = spawn(fun F() -> receive hej -> spawn_link(fun()-> receive M -> M end end), F() end end),
    timer:sleep(100),
    link(TraceeChild3),
    timer:sleep(100),
    TraceeChild3 ! hej,
    timer:sleep(100),
    unlink(TraceeChild3),
    timer:sleep(100),
    exit(TraceeChild3, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer,{trace, Tracee, spawn,TraceeChild3,'_'}},
         {Tracer,{trace, Tracee, link, TraceeChild3}},
         {Tracer,{trace, TraceeChild3, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer,{trace, TraceeChild3, spawn,'_','_'}},
         {Tracer,{trace, TraceeChild3, link, '_'}}]),
    receive_unsorted(
        [{Tracer,{trace, Tracee, unlink, TraceeChild3}},
         {Tracer,{trace, TraceeChild3, getting_unlinked, Tracee}},
         {Tracer,{trace, TraceeChild3, exit, die}}]),
    1 = erlang:trace(self(), false, [procs, set_on_first_link | Opts]),
    ok.

set_on_first_link2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_link | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_first_link | Opts2]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    TraceeChild1 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer2,{trace, Tracee, spawn,TraceeChild,'_'}},
         {Tracer1,{trace, Tracee, link, TraceeChild}},
         {Tracer2,{trace, Tracee, link, TraceeChild}},
         {Tracer1,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer2,{trace, TraceeChild, spawned,Tracee,'_'}},
         {Tracer1,{trace, TraceeChild, getting_linked, Tracee}},
         {Tracer2,{trace, TraceeChild, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, spawn,TraceeChild1,'_'}},
         {Tracer2,{trace, Tracee, spawn,TraceeChild1,'_'}},
         {Tracer1,{trace, Tracee, link, TraceeChild1}},
         {Tracer2,{trace, Tracee, link, TraceeChild1}}]),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, unlink, TraceeChild}},
         {Tracer2,{trace, Tracee, unlink, TraceeChild}},
         {Tracer1,{trace, TraceeChild, getting_unlinked, Tracee}},
         {Tracer2,{trace, TraceeChild, getting_unlinked, Tracee}}]),
    receive_unsorted(
        [{Tracer1,{trace, TraceeChild, exit, die}},
         {Tracer2,{trace, TraceeChild, exit, die}}]),
    receive_unsorted(
        [{Tracer1,{trace, Tracee, unlink, TraceeChild1}},
         {Tracer2,{trace, Tracee, unlink, TraceeChild1}}]),

    1 = erlang:trace(self(), false, [procs, set_on_first_link | Opts1]),
    TraceeChild2 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    TraceeChild21 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild2),
    timer:sleep(100),
    exit(TraceeChild2, die),
    timer:sleep(100),
    unlink(TraceeChild21),
    timer:sleep(100),
    exit(TraceeChild21, die),
    timer:sleep(100),
    {Tracer2,{trace, Tracee, spawn,TraceeChild2,_}} = receive_any(),
    {Tracer2,{trace, Tracee, link, TraceeChild2}} = receive_any(),
    {Tracer2,{trace, Tracee, spawn,TraceeChild21,_}} = receive_any(),
    {Tracer2,{trace, Tracee, link, TraceeChild21}} = receive_any(),
    {Tracer2,{trace, Tracee, unlink, TraceeChild2}} = receive_any(),
    {Tracer2,{trace, Tracee, unlink, TraceeChild21}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_first_link | Opts2]),
    ok.
procs(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),

    procs_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    procs_do2(Tracer1, [{session,S1}],
              Tracer2, [{session,S2}]),
    procs_do2(Tracer0, [{tracer,Tracer0}],
              Tracer2, [{session,S2}]),
    procs_do2(Tracer2, [{session,S2}],
              Tracer0, [{tracer,Tracer0}]),

    procs_do2(Tracer1, [{session,S1}],
              Tracer0, [{tracer,Tracer0},{session,S0}]),
    procs_do2(Tracer0, [{tracer,Tracer0},{session,S0}],
              Tracer1, [{session,S1}]),

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
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    {Tracer, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer, {trace, Tracee, unregister, RegName}} = receive_any(),
    receive_unsorted(
      [{Tracer, {trace, Tracee, spawn, TraceeChild, '_'}},
       {Tracer, {trace, TraceeChild, spawned, Tracee, '_'}},
       {Tracer, {trace, Tracee, link, TraceeChild}},
       {Tracer, {trace, TraceeChild, getting_linked, Tracee}}]),
    receive_unsorted(
      [{Tracer, {trace, Tracee, unlink, TraceeChild}},
       {Tracer, {trace, TraceeChild, getting_unlinked, Tracee}}]),
    {Tracer, {trace, TraceeChild, exit, _Reason}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts]),

    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild1),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),

    timeout = receive_any(),

    ok.
procs_do2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts2]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    receive_unsorted(
        [{Tracer1, {trace, Tracee, register, RegName}},
         {Tracer2, {trace, Tracee, register, RegName}}]),
    receive_unsorted(
        [{Tracer1, {trace, Tracee, unregister, RegName}},
         {Tracer2, {trace, Tracee, unregister, RegName}}]),
    receive_unsorted(
        [{Tracer1, {trace, Tracee, spawn, TraceeChild, '_'}},
         {Tracer2, {trace, Tracee, spawn, TraceeChild, '_'}},
         {Tracer1, {trace, TraceeChild, spawned, Tracee, '_'}},
         {Tracer2, {trace, TraceeChild, spawned, Tracee, '_'}},
         {Tracer1, {trace, Tracee, link, TraceeChild}},
         {Tracer2, {trace, Tracee, link, TraceeChild}},
         {Tracer1, {trace, TraceeChild, getting_linked, Tracee}},
         {Tracer2, {trace, TraceeChild, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer1, {trace, Tracee, unlink, TraceeChild}},
         {Tracer2, {trace, Tracee, unlink, TraceeChild}},
         {Tracer1, {trace, TraceeChild, getting_unlinked, Tracee}},
         {Tracer2, {trace, TraceeChild, getting_unlinked, Tracee}}]),
    receive_unsorted(
        [{Tracer1, {trace, TraceeChild, exit, '_'}},
         {Tracer2, {trace, TraceeChild, exit, '_'}}]),

    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts1]),
    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild1),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),
    {Tracer2, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer2, {trace, Tracee, unregister, RegName}} = receive_any(),
    receive_unsorted(
        [{Tracer2, {trace, Tracee, spawn, TraceeChild1, '_'}},
         {Tracer2, {trace, TraceeChild1, spawned, Tracee, '_'}},
         {Tracer2, {trace, Tracee, link, TraceeChild1}},
         {Tracer2, {trace, TraceeChild1, getting_linked, Tracee}}]),
    receive_unsorted(
        [{Tracer2, {trace, Tracee, unlink, TraceeChild1}},
         {Tracer2, {trace, TraceeChild1, getting_unlinked, Tracee}}]),
    {Tracer2, {trace, TraceeChild1, exit, _}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts2]),
    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild2 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild2),
    timer:sleep(100),
    unlink(TraceeChild2),
    timer:sleep(100),
    exit(TraceeChild2, die),
    timeout = receive_any(),
    ok.

basic(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0",Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2",Tester) end),

    basic_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    basic_do2(Tracer1, [{session,S1}],
              Tracer2, [{session,S2}]),
    basic_do2(Tracer0, [{tracer,Tracer0}],
              Tracer2, [{session,S2}]),
    basic_do2(Tracer2, [{session,S2}],
              Tracer0, [{tracer,Tracer0}]),

    basic_do2(Tracer1, [{session,S1}],
              Tracer0, [{tracer,Tracer0},{session,S0}]),
    basic_do2(Tracer0, [{tracer,Tracer0},{session,S0}],
              Tracer1, [{session,S1}]),

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

    timeout = receive_any(),

    ok.

basic_do2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs | Opts1]),
    1 = erlang:trace(self(), true, [procs | Opts2]),

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

    1 = erlang:trace(self(), false, [procs | Opts1]),

    register(RegName, Tracee),
    unregister(RegName),

    {Tracer2, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer2, {trace, Tracee, unregister, RegName}} = receive_any(),

    1 = erlang:trace(self(), false, [procs | Opts2]),

    register(RegName, Tracee),
    unregister(RegName),

    timeout = receive_any(),

    ok.


call(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0",Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2",Tester) end),

    call_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    [begin
         io:format("CallType = ~p\n", [CallType]),
         call_do2(Tracer1, [{session,S1}],
                  Tracer2, [{session,S2}], CallType),
         call_do2(Tracer0, [{tracer,Tracer0}],
                  Tracer2, [{session,S2}], CallType),
         call_do2(Tracer2, [{session,S2}],
                  Tracer0, [{tracer,Tracer0}], CallType),

         call_do2(Tracer1, [{session,S1}],
                  Tracer0, [{tracer,Tracer0},{session,S0}], CallType),
         call_do2(Tracer0, [{tracer,Tracer0},{session,S0}],
                  Tracer1, [{session,S1}], CallType)
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
    1 = erlang:trace_pattern(MFArity, true, add_session([local], Opts)),

    foo(),

    {Tracer, {trace, Tracee, call, MFArgs}} = receive_any(),

    1 = erlang:trace(self(), false, [call | Opts]),

    foo(),
    timeout = receive_any(),

    ok.

call_do2(Tracer1, Opts1, Tracer2, Opts2, {TPopt, Call}) ->
    MFArity = {?MODULE,foo,0},
    MFArgs = {?MODULE,foo,[]},
    Tracee = self(),
    1 = erlang:trace(Tracee, true, [call | Opts1]),
    1 = erlang:trace(Tracee, true, [call | Opts2]),

    ?line,
    1 = erlang:trace_pattern(MFArity, true, add_session(TPopt, Opts1)),
    Call(),
    {Tracer1, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    ?line,
    1 = erlang:trace_pattern(MFArity, true, add_session(TPopt, Opts2)),
    Call(),
    {Ta, {trace, Tracee, call, MFArgs}} = receive_any(),
    {Tb, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),
    #{Tracer1 := v, Tracer2 := v} = #{Ta => v, Tb => v},


    ?line,
    1 = erlang:trace_pattern(MFArity, false, add_session(TPopt, Opts2)),
    Call(),
    {Tracer1, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    ?line,
    1 = erlang:trace_pattern(MFArity, true, add_session(TPopt, Opts2)),
    Call(),
    {Tc, {trace, Tracee, call, MFArgs}} = receive_any(),
    {Td, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),
    #{Tracer1 := v, Tracer2 := v} = #{Tc => v, Td => v},

    ?line,
    1 = erlang:trace_pattern(MFArity, false, add_session(TPopt, Opts1)),
    Call(),
    {Tracer2, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    ?line,
    1 = erlang:trace_pattern(MFArity, true, add_session(TPopt, Opts1)),
    Call(),
    {Te, {trace, Tracee, call, MFArgs}} = receive_any(),
    {Tf, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),
    #{Tracer1 := v, Tracer2 := v} = #{Te => v, Tf => v},

    ?line,
    1 = erlang:trace(self(), false, [call | Opts1]),
    Call(),
    {Tracer2, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    ?line,
    1 = erlang:trace_pattern(MFArity, false, add_session(TPopt, Opts2)),
    Call(),
    timeout = receive_any(),

    ?line,
    1 = erlang:trace_pattern(MFArity, false, add_session(TPopt, Opts1)),
    1 = erlang:trace(self(), false, [call | Opts2]),
    Call(),
    timeout = receive_any(),

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
    timeout = receive_any(),

    unlink(Tracer0),
    exit(Tracer0, die),

    ok.

foo() ->
    ok.

add_session(DstOpts, SrcOpts) ->
    case lists:keyfind(session, 1, SrcOpts) of
        {session, _}=S ->
            [S | DstOpts];
        false ->
            DstOpts
    end.

tracer(Name, Tester) ->
    receive M ->
            io:format("~p ~p got message: ~p\n", [Name, self(), M]),
            Tester ! {self(), M}
    end,
    tracer(Name, Tester).


receive_any() ->
    receive_any(100).

receive_any(Timeout) ->
    receive M -> M
    after Timeout -> timeout
    end.

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
