%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(dyntrace_lttng_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([t_lttng_list/1,
         t_procs/1,
         t_ports/1,
         t_running_process/1,
         t_running_port/1,
         t_call/1,
         t_call_return_to/1,
         t_call_silent/1,
         t_send/1,
         t_receive/1,
         t_garbage_collection/1,
         t_all/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 10}}].

all() ->
    [t_lttng_list,
     t_procs,
     t_ports,
     t_running_process,
     t_running_port,
     t_call,
     t_call_return_to,
     t_call_silent,
     t_send,
     t_receive,
     t_garbage_collection,
     t_all].


init_per_suite(Config) ->
    case erlang:system_info(dynamic_trace) of
        lttng ->
            ensure_lttng_stopped("--all"),
            Config;
        _ ->
            {skip, "No LTTng configured on system."}
    end.

end_per_suite(_Config) ->
    ensure_lttng_stopped("--all"),
    ok.

init_per_testcase(Case, Config) ->
    %% ensure loaded
    _ = dyntrace:module_info(),
    Name = atom_to_list(Case),
    ok = ensure_lttng_started(Name, Config),
    [{session, Name}|Config].

end_per_testcase(Case, _Config) ->
    Name = atom_to_list(Case),
    ok = ensure_lttng_stopped(Name),
    ok.

%% tracepoints
%%
%%  org_erlang_dyntrace:gc_major_end
%%  org_erlang_dyntrace:gc_major_start
%%  org_erlang_dyntrace:gc_minor_end
%%  org_erlang_dyntrace:gc_minor_start
%%  org_erlang_dyntrace:message_receive
%%  org_erlang_dyntrace:message_send
%%  -org_erlang_dyntrace:message_queued
%%  org_erlang_dyntrace:function_exception
%%  org_erlang_dyntrace:function_return
%%  org_erlang_dyntrace:function_call
%%  org_erlang_dyntrace:port_link
%%  org_erlang_dyntrace:port_exit
%%  org_erlang_dyntrace:port_open
%%  org_erlang_dyntrace:port_scheduled
%%  org_erlang_dyntrace:process_scheduled
%%  org_erlang_dyntrace:process_register
%%  org_erlang_dyntrace:process_exit
%%  org_erlang_dyntrace:process_link
%%  org_erlang_dyntrace:process_spawn
%%
%% Testcases
%%

t_lttng_list(_Config) ->
    {ok, _} = cmd("lttng list -u"),
    ok.

t_procs(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:process_*", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},procs]),

    Pid = spawn_link(fun() -> waiter() end),
    Pid ! {self(), ok},
    ok = receive {Pid,ok} -> ok end,
    timer:sleep(1000),

    _ = erlang:trace(all, false, [procs]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:process_spawn", Res),
    ok = check_tracepoint("org_erlang_dyntrace:process_link", Res),
    ok = check_tracepoint("org_erlang_dyntrace:process_exit", Res),
    ok = check_tracepoint("org_erlang_dyntrace:process_register", Res),
    ok.

t_ports(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:port_*", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},ports]),

    _ = os:cmd("ls"),

    _ = erlang:trace(all, false, [{tracer, dyntrace, []},ports]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:port_open", Res),
    ok = check_tracepoint("org_erlang_dyntrace:port_link", Res),
    ok = check_tracepoint("org_erlang_dyntrace:port_exit", Res),
    ok.

t_running_process(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:process_scheduled", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},running]),

    Pid = spawn_link(fun() -> waiter() end),
    Pid ! {self(), ok},
    ok = receive {Pid,ok} -> ok end,
    timer:sleep(1000),

    _ = erlang:trace(all, false, [running]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:process_scheduled", Res),
    ok.

t_running_port(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:port_scheduled", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},running_ports]),

    _ = os:cmd("ls"),
    _ = os:cmd("ls"),

    _ = erlang:trace(all, false, [running_ports]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:port_scheduled", Res),
    ok.


t_call(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:function_*", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []}, call]),
    _ = erlang:trace_pattern({?MODULE, '_', '_'}, [{'_',[],[{exception_trace}]}], [local]),

    DontLink = spawn(fun() -> foo_clause_exception(nope) end),
    Pid = spawn_link(fun() -> waiter() end),
    Pid ! {self(), ok},
    ok = receive {Pid,ok} -> ok end,
    
    timer:sleep(10),
    undefined = erlang:process_info(DontLink),

    _ = erlang:trace_pattern({?MODULE, '_', '_'}, false, [local]),
    _ = erlang:trace(all, false, [call]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:function_call", Res),
    ok = check_tracepoint("org_erlang_dyntrace:function_return", Res),
    ok = check_tracepoint("org_erlang_dyntrace:function_exception", Res),
    ok.

t_send(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:message_send", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},send]),

    Pid = spawn_link(fun() -> waiter() end),
    Pid ! {self(), ok},
    ok = receive {Pid,ok} -> ok end,
    _ = os:cmd("ls"),
    timer:sleep(10),

    _ = erlang:trace(all, false, [send]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:message_send", Res),
    ok.

t_call_return_to(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:function_*", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []}, call, return_to]),
    _ = erlang:trace_pattern({lists, '_', '_'}, true, [local]),
    _ = erlang:trace_pattern({?MODULE, '_', '_'}, true, [local]),

    Pid = spawn_link(fun() -> gcfier(10) end),
    Pid ! {self(), ok},
    ok = receive {Pid,ok} -> ok end,
    timer:sleep(10),

    _ = erlang:trace_pattern({?MODULE, '_', '_'}, false, [local]),
    _ = erlang:trace_pattern({lists, '_', '_'}, false, [local]),
    _ = erlang:trace(all, false, [call,return_to]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:function_call", Res),
    ok.

t_call_silent(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:function_*", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []}, call, silent]),
    _ = erlang:trace_pattern({?MODULE, '_', '_'}, [{'_',[],[{exception_trace}]}], [local]),

    DontLink = spawn(fun() -> foo_clause_exception(nope) end),
    Pid = spawn_link(fun() -> waiter() end),
    Pid ! {self(), ok},
    ok = receive {Pid,ok} -> ok end,

    timer:sleep(10),
    undefined = erlang:process_info(DontLink),

    _ = erlang:trace_pattern({?MODULE, '_', '_'}, false, [local]),
    _ = erlang:trace(all, false, [call]),
    Res = lttng_stop_and_view(Config),
    notfound = check_tracepoint("org_erlang_dyntrace:function_call", Res),
    notfound = check_tracepoint("org_erlang_dyntrace:function_return", Res),
    notfound = check_tracepoint("org_erlang_dyntrace:function_exception", Res),
    ok.


t_receive(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:message_receive", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},'receive']),
    timer:sleep(20),

    Pid1 = spawn_link(fun() -> waiter() end),
    Pid1 ! {self(), ok},
    ok = receive {Pid1,ok} -> ok end,

    Pid2 = spawn_link(fun() -> waiter() end),
    Pid2 ! {self(), ok},
    ok = receive {Pid2,ok} -> ok end,

    timer:sleep(10),
    _ = erlang:trace(all, false, ['receive']),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:message_receive", Res),
    ok.

t_garbage_collection(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:gc_*", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},garbage_collection]),

    Pid = spawn_link(fun() -> gcfier() end),
    Pid ! {self(), ok},
    ok = receive {Pid,ok} -> ok end,
    timer:sleep(10),
    _ = erlang:trace(all, false, [garbage_collection]),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_dyntrace:gc_major_start", Res),
    ok = check_tracepoint("org_erlang_dyntrace:gc_major_end", Res),
    ok = check_tracepoint("org_erlang_dyntrace:gc_minor_start", Res),
    ok = check_tracepoint("org_erlang_dyntrace:gc_minor_end", Res),
    ok.

t_all(Config) when is_list(Config) ->
    ok = lttng_start_event("org_erlang_dyntrace:*", Config),
    _ = erlang:trace(new, true, [{tracer, dyntrace, []},all]),

    Pid1 = spawn_link(fun() -> waiter() end),
    Pid1 ! {self(), ok},
    ok = receive {Pid1,ok} -> ok end,

    Pid2 = spawn_link(fun() -> gcfier() end),
    Pid2 ! {self(), ok},
    ok = receive {Pid2,ok} -> ok end,
    _ = os:cmd("ls"),
    _ = os:cmd("ls"),
    timer:sleep(10),

    _ = erlang:trace(all, false, [all]),
    Res = lttng_stop_and_view(Config),

    ok = check_tracepoint("org_erlang_dyntrace:process_spawn", Res),
    ok = check_tracepoint("org_erlang_dyntrace:process_link", Res),
    ok = check_tracepoint("org_erlang_dyntrace:process_exit", Res),
    ok = check_tracepoint("org_erlang_dyntrace:process_register", Res),
    ok = check_tracepoint("org_erlang_dyntrace:port_open", Res),
    ok = check_tracepoint("org_erlang_dyntrace:port_link", Res),
    ok = check_tracepoint("org_erlang_dyntrace:port_exit", Res),
    ok = check_tracepoint("org_erlang_dyntrace:process_scheduled", Res),
    ok = check_tracepoint("org_erlang_dyntrace:port_scheduled", Res),
    ok = check_tracepoint("org_erlang_dyntrace:message_send", Res),
    ok = check_tracepoint("org_erlang_dyntrace:message_receive", Res),
    ok = check_tracepoint("org_erlang_dyntrace:gc_major_start", Res),
    ok = check_tracepoint("org_erlang_dyntrace:gc_major_end", Res),
    ok = check_tracepoint("org_erlang_dyntrace:gc_minor_start", Res),
    ok = check_tracepoint("org_erlang_dyntrace:gc_minor_end", Res),
    ok.


%% aux

gcfier() ->
    gcfier(10000).
gcfier(N) ->
    receive
        {Pid, ok} ->
            _ = lists:reverse(lists:seq(1,N)),
            true = erlang:garbage_collect(),
            Pid ! {self(), ok}
    end.


waiter() ->
    true = register(?MODULE, self()),
    receive
        {Pid, ok} ->
            Child = spawn(fun() -> receive ok -> ok end end),
            link(Child),
            unlink(Child),
            _ = lists:seq(1,1000),
            Child ! ok,
            true = unregister(?MODULE),
            Pid ! {self(),ok}
    end.

foo_clause_exception({1,2}) -> badness.

%% lttng
lttng_stop_and_view(Config) ->
    Path = proplists:get_value(priv_dir, Config),
    Name = proplists:get_value(session, Config),
    {ok,_} = cmd("lttng stop " ++ Name),
    {ok,Res} = cmd("lttng view " ++ Name ++ " --trace-path=" ++ Path),
    Res.

check_tracepoint(TP, Data) ->
    case re:run(Data, TP, [global]) of
        {match, _} -> ok;
        _ -> notfound
    end.

lttng_start_event(Event, Config) ->
    Name = proplists:get_value(session, Config),
    {ok, _} = cmd("lttng enable-event -u " ++ Event ++ " --session=" ++ Name),
    {ok, _} = cmd("lttng start " ++ Name),
    ok.

ensure_lttng_started(Name, Config) ->
    Out = case proplists:get_value(priv_dir, Config) of
              undefined -> [];
              Path -> "--output="++Path++" "
          end,
    {ok,_} = cmd("lttng create " ++ Out ++ Name),
    ok.

ensure_lttng_stopped(Name) ->
    {ok,_} = cmd("lttng stop"),
    {ok,_} = cmd("lttng destroy " ++ Name),
    ok.

cmd(Cmd) ->
    io:format("<< ~ts~n", [Cmd]),
    Res = os:cmd(Cmd),
    io:format(">> ~ts~n", [Res]),
    {ok,Res}.
