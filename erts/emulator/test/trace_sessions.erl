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

-module(trace_sessions).

%%
%% This is NOT a test suite.
%% It's some common code shared by trace test suites
%% in order to group testcases and repeat them with different
%% usage of trace sessions.
%%

-export([all/0, groups/1,
         init_per_suite/1, end_per_suite/1, suite_controller/2,
         init_per_group/2, end_per_group/2,
         init_per_testcase/1, end_per_testcase/1,
         erlang_trace/3,
         erlang_trace_info/2,
         erlang_trace_pattern/2,
         erlang_trace_pattern/3
        ]).

all() ->
    [{group, legacy},
     {group, legacy_pre_session},
     {group, legacy_post_session},
     {group, dynamic_session}].

groups(Testcases) ->
    [{legacy, [], Testcases},
     {legacy_pre_session, [], Testcases},
     {legacy_post_session, [], Testcases},
     {dynamic_session, [], Testcases}].

init_per_suite(Config) ->
    Pid = spawn(?MODULE, suite_controller, [start, []]),
    [{suite_controller, Pid} | Config].

end_per_suite(Config) ->
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

erlang_trace_pattern(MFA, MS, FlagList0) ->
    FlagList1 =
        case ets:lookup(?MODULE, dynamic_session) of
            [] -> FlagList0;
            [{dynamic_session, DynS}] ->
                [{session,DynS} | FlagList0]
        end,

    R = erlang:trace_pattern(MFA, MS, FlagList1),
    io:format("trace_pattern(~p, ~p, ~p) -> ~p\n", [MFA, MS, FlagList1, R]),

    case ets:lookup(?MODULE, legacy_post_session) of
        [] -> R;
        [{legacy_post_session, S, _}] ->
            On = case MS of
                     false -> false;
                     _ -> true
                 end,
            case MFA of
                {_,_,_} ->
                    erlang:trace_pattern(MFA, On, [call_count, {session,S}]);
                _ ->
                    ok %% send & receive trace already turned off
            end,
            R
    end.

global_local([]) -> global;
global_local([global]) -> global;
global_local(_) -> local.

%% Wrap erlang:trace/3
erlang_trace(PidPortSpec, How, FlagList0) ->
    FlagList1 =
        case ets:lookup(?MODULE, dynamic_session) of
            [] ->
                FlagList0;
            [{dynamic_session, S}] ->
                [{session, S} | FlagList0]
        end,
    erlang:trace(PidPortSpec, How, FlagList1).


%% Wrap erlang:trace_info/2
erlang_trace_info(PidPortFuncEvent, Item) ->
    case ets:lookup(?MODULE, dynamic_session) of
        [] ->
            erlang:trace_info(PidPortFuncEvent, Item);
        [{dynamic_session, S}] ->
            erlang:trace_info(S, PidPortFuncEvent, Item)
    end.

init_per_group(legacy, Config) ->
    %% Run tests using only the default legacy trace session.
    Config;
init_per_group(legacy_pre_session, Config) ->
    %% Run tests using default legacy session
    %% but create an omnipresent dynamic dummy session before.
    Tracer = proplists:get_value(suite_controller, Config),
    S = erlang:trace_session_create([{tracer,Tracer}]),

    %% Set a dummy call_count on all (local) functions.
    erlang:trace_pattern({'_','_','_'}, true, [local, {session,S}]),

    %% Re-set a dummy global call trace on all exported functions.
    [[erlang:trace_pattern({Module, Func, Arity}, true, [global, {session,S}])
      || {Func,Arity} <- Module:module_info(exports)]
     || Module <- erlang:loaded(),
        erlang:function_exported(Module, module_info, 1)],

    %% Set a dummy send trace on all processes and ports
    %% but disable send trace to not get any messages.
    erlang:trace(all, true, [send, {session,S}]),
    1 = erlang:trace_pattern(send, false, [{session,S}]),

    [{dummy_session, {S, Tracer}} | Config];
init_per_group(legacy_post_session, Config) ->
    %% Run tests using default legacy session
    %% but create a dynamic dummy session after
    Tracer = proplists:get_value(suite_controller, Config),
    S = erlang:trace_session_create([{tracer, Tracer}]),
    1 = erlang:trace_pattern(send, false, [{session,S}]),
    1 = erlang:trace_pattern('receive', false, [{session,S}]),
    ets:insert(?MODULE, {legacy_post_session, S, Tracer}),
    Config;
init_per_group(dynamic_session, Config) ->
    %% Run tests with a dynamically created session.
    S = erlang:trace_session_create([]),
    ets:insert(?MODULE, {dynamic_session, S}),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(legacy_pre_session, Config) ->
    {S, Tracer} = proplists:get_value(dummy_session, Config),
    true = is_process_alive(Tracer),
    erlang:trace_session_destroy(S),
    Config;
end_per_group(legacy_post_session, Config) ->
    [{legacy_post_session, S, Tracer}] = ets:take(?MODULE, legacy_post_session),
    true = is_process_alive(Tracer),
    erlang:trace_session_destroy(S),
    Config;
end_per_group(dynamic_session, Config) ->
    [{dynamic_session, S}] = ets:take(?MODULE, dynamic_session),
    erlang:trace_session_destroy(S),
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(Config) ->
    Config.

end_per_testcase(Config) ->
    suite_controller_check(Config).
