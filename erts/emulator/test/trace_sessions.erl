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

-module(trace_sessions).

%%
%% This is NOT a test suite.
%% It's some common code shared by trace test suites
%% in order to group testcases and repeat them with different
%% usage of trace sessions.
%%

-export([all/0, groups/1,
         init_per_suite/2, end_per_suite/1, suite_controller/2,
         init_per_group/2, end_per_group/2,
         init_per_testcase/1, end_per_testcase/1,
         erlang_trace/3,
         erlang_trace_info/2,
         erlang_trace_pattern/2,
         erlang_trace_pattern/3,
         erlang_system_monitor/0,
         erlang_system_monitor/1,
         erlang_system_monitor/2
        ]).

group_map() ->
    #{%%legacy => [],
      %%legacy_pre_session => [pre_session],
      %%legacy_post_session => [post_session],
      legacy_pre_post => [pre_session, post_session],
      %%dynamic_session => [dynamic_session]
      dynamic_pre_post => [pre_session, post_session, dynamic_session]
     }.

group_list() ->
    maps:keys(group_map()).

all() ->
    [{group, Group} || Group <- group_list()].

groups(Testcases) ->
    [{Group, [], Testcases} || Group <- group_list()].

init_per_suite(Config, SuiteModule) ->
    SessionsBefore = trace:session_info(all),
    Pid = spawn(?MODULE, suite_controller, [start, []]),
    [{suite_controller, Pid},
     {sessions_before, SessionsBefore},
     {suite_module, SuiteModule}
    | Config].

end_per_suite(Config) ->
    SessionsBefore = proplists:get_value(sessions_before, Config),
    SessionsAfter = trace:session_info(all),
    Diff = SessionsAfter -- SessionsBefore,
    io:format("SessionsBefore = ~p\n", [SessionsBefore]),
    io:format("SessionsAfter = ~p\n", [SessionsAfter]),
    io:format("Diff = ~p\n", [Diff]),
    [] = Diff,
    Pid = proplists:get_value(suite_controller, Config),
    true = is_process_alive(Pid),
    exit(Pid, kill),
    ok.

%% The suite controller process serves two purposes:
%% 1. Keep the suite ETS table alive.
%% 2. Act as tracer for dummy sessions that should not get any trace messages.
suite_controller(start, []) ->
    ets:new(?MODULE, [public, named_table]),
    suite_controller(loop, []);
suite_controller(loop, Acc0) ->
    Acc1 = receive
               {flush, Pid} ->
                   Pid ! {self(), Acc0},
                   [];
               Unexpected ->
                   erlang:display({unexpected,Unexpected}),
                   io:format("Dummy tracer got unexpected message:\n~p\n",
                             [Unexpected]),
                   [Unexpected | Acc0]
           end,
    ?MODULE:suite_controller(loop, Acc1).

suite_controller_check(Config) ->
    Pid = proplists:get_value(suite_controller, Config),
    true = is_process_alive(Pid),
    Pid ! {flush, self()},
    case receive {Pid, List} -> List end of
        [] -> true;
        _ -> {fail, "Unexpected trace messages"}
    end.

%% Wrap erlang:trace_pattern/2/3
%% but with some session tricks depending on test group.
erlang_trace_pattern(MFA, MS) ->
    erlang_trace_pattern(MFA, MS, []).

erlang_trace_pattern(MFA, MS, FlagList) ->
    case ets:lookup(?MODULE, dynamic_session) of
        [] ->
            R = erlang:trace_pattern(MFA, MS, FlagList),
            io:format("trace_pattern(~p, ~p, ~p) -> ~p\n",
                      [MFA, MS, FlagList, R]);

        [{dynamic_session, DynS}] ->
            R = case MFA of
                    {_,_,_} ->
                        trace:function(DynS, MFA, MS, FlagList);
                    on_load ->
                        trace:function(DynS, on_load, MS, FlagList);
                    send ->
                        trace:send(DynS, MS, FlagList);
                    'receive' ->
                        trace:recv(DynS, MS, FlagList)
                end,
            io:format("trace_pattern(~p, ~p, ~p, ~p) -> ~p\n",
                      [DynS, MFA, MS, FlagList, R])

        end,

    case ets:lookup(?MODULE, post_session) of
        [] -> R;
        [{post_session, S, _}] ->
            On = case MS of
                     false -> false;
                     _ -> true
                 end,
            case MFA of
                {_,_,_} ->
                    trace:function(S, MFA, On, [call_count]);
                _ ->
                    ok %% send & receive trace already turned off
            end,
            R
    end.

%% Wrap erlang:trace/3
erlang_trace(PidPortSpec, How, FlagList0) ->
    case ets:lookup(?MODULE, dynamic_session) of
        [] ->
            erlang:trace(PidPortSpec, How, FlagList0);
        [{dynamic_session, S}] ->
            case handle_cpu_timestamp(PidPortSpec, How, FlagList0) of
                {true, Ret, []} ->
                    Ret;
                {_, _, FlagList1} ->
                    if is_pid(PidPortSpec);
                       PidPortSpec =:= processes;
                       PidPortSpec =:= existing_processes;
                       PidPortSpec =:= new_processes ->
                            trace:process(S, PidPortSpec, How, FlagList1);

                       is_port(PidPortSpec);
                       PidPortSpec =:= ports;
                       PidPortSpec =:= existing_ports;
                       PidPortSpec =:= new_ports ->
                            trace:port(S, PidPortSpec, How, FlagList1);

                       PidPortSpec =:= all;
                       PidPortSpec =:= existing;
                       PidPortSpec =:= new ->
                            trace:process(S, PidPortSpec, How, FlagList1),
                            trace:port(S, PidPortSpec, How, FlagList1);

                       true ->
                            %% Must be negative testing
                            trace:process(S, PidPortSpec, How, FlagList1)
                    end
            end
    end.

handle_cpu_timestamp(all, How, FlagList) ->
    case lists:member(cpu_timestamp, FlagList) of
        true ->
            %% Do special call for ugly duckling 'cpu_timestamp'
            %% not (yet) supported by module 'trace'.
            Ret = erlang:trace(all, How, [cpu_timestamp]),
            FlagListRest = lists:filter(fun(E) -> E =/= cpu_timestamp end,
                                        FlagList),
            {true, Ret, FlagListRest};

        false ->
            {false, void, FlagList}
    end;
handle_cpu_timestamp(_, _, FlagList) ->
    {false, void, FlagList}.



%% Wrap erlang:trace_info/2
erlang_trace_info(PidPortFuncEvent, Item) ->
    case ets:lookup(?MODULE, dynamic_session) of
        [] ->
            erlang:trace_info(PidPortFuncEvent, Item);
        [{dynamic_session, S}] ->
            trace:info(S, PidPortFuncEvent, Item)
    end.

erlang_system_monitor() ->
    case ets:lookup(?MODULE, dynamic_session) of
        [] ->
            erlang:system_monitor();
        [{dynamic_session, S}] ->
            erts_internal:system_monitor(S)
    end.

erlang_system_monitor(undefined) ->
    erlang_system_monitor(undefined, []);
erlang_system_monitor({Pid, Opts}) ->
    erlang_system_monitor(Pid, Opts).

erlang_system_monitor(Pid, Opts) ->
    case ets:lookup(?MODULE, dynamic_session) of
        [] ->
            erlang:system_monitor(Pid, Opts);
        [{dynamic_session, S}] ->
            erts_internal:system_monitor(S, Pid, Opts)
    end.

init_per_group(Group, Config) ->
    init_group(group_tricks(Group), Config).

end_per_group(Group, Config) ->
    end_group(group_tricks(Group), Config).

group_tricks(Group) ->
    maps:get(Group, group_map(), []).

init_group([], Config) ->
    Config;
init_group([pre_session|Tail], Config) ->
    %%
    %% Create an omnipresent dynamic dummy session before.
    %%
    Tracer = proplists:get_value(suite_controller, Config),
    S = trace:session_create(pre_session, Tracer, []),

    %% Set a dummy call_count on all (local) functions.
    trace:function(S, {'_','_','_'}, true, [local]),

    %% Re-set a dummy global call trace on exported functions.
    set_dummy_global_trace(S, Config),

    %% Set a dummy send trace on all processes and ports
    %% but disable send trace to not get any messages.
    1 = trace:send(S, false, []),
    trace:process(S, all, true, [send]),
    trace:port(S, all, true, [send]),

    ets:insert(?MODULE, {pre_session, S, Tracer}),
    init_group(Tail, Config);
init_group([post_session | Tail], Config) ->
    %%
    %% Create a dynamic dummy session after
    %%
    Tracer = proplists:get_value(suite_controller, Config),
    S = trace:session_create(post_session, Tracer, []),
    1 = trace:send(S, false, []),
    1 = trace:recv(S, false, []),
    ets:insert(?MODULE, {post_session, S, Tracer}),
    init_group(Tail, Config);
init_group([dynamic_session | Tail], Config) ->
    %%
    %% Run tests with a dynamically created session.
    %%
    %% Note: The 'undefined' tracer argument used here is an *undocumented*
    %% feature kept for this test purpose. That is, to reuse all existing trace
    %% test suites but rerun them with a dynamic session that behaves like the
    %% legacy session and allows setting tracer per traced process/port.
    %%
    S = trace:session_create(dynamic_session, undefined, []),
    ets:insert(?MODULE, {dynamic_session, S}),
    init_group(Tail, Config).

end_group([], Config) ->
    Config;
end_group([pre_session | Tail], Config) ->
    [{pre_session, S, Tracer}] = ets:take(?MODULE, pre_session),
    true = is_process_alive(Tracer),
    trace:session_destroy(S),
    end_group(Tail, Config);
end_group([post_session | Tail], Config) ->
    [{post_session, S, Tracer}] = ets:take(?MODULE, post_session),
    true = is_process_alive(Tracer),
    trace:session_destroy(S),
    end_group(Tail, Config);
end_group([dynamic_session | Tail], Config) ->
    [{dynamic_session, S}] = ets:take(?MODULE, dynamic_session),
    trace:session_destroy(S),
    end_group(Tail, Config).

init_per_testcase(Config) ->
    Config.

end_per_testcase(Config) ->
    suite_controller_check(Config).



set_dummy_global_trace(S, Config) ->
    %% Modules = [M || M <- erlang:loaded(),
    %%                 erlang:function_exported(M, module_info, 1)],
    %% Calling trace_pattern on all loaded modules takes too long time
    %% on some machines. Do it on a subset of modules instead.

    %% Set a dummy global call trace on some exported functions.
    SuiteModule = proplists:get_value(suite_module, Config),
    Modules = [erlang, ets, lists, maps, SuiteModule],
    io:format("~w modules to trace...\n", [length(Modules)]),

    [begin
         {Micros, 1} = timer:tc(fun() ->
                                        trace:function(S, {Mod, Func, Arity},
                                                       true, [global])
                                end),
         io:format("~10w: ~w:~w/~w", [Micros, Mod, Func, Arity])
     end
     || Mod <- Modules,
        {Func, Arity} <- Mod:module_info(exports)],

    ok.
