%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

-module(tracer_SUITE).

%%%
%%% Tests the tracer module interface
%%%

-export([all/0, suite/0,groups/0, init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
-export([load/1, unload/1, reload/1, invalid_tracers/1]).
-export([send/1, recv/1, spawn/1, exit/1, link/1, unlink/1,
         getting_linked/1, getting_unlinked/1, register/1, unregister/1,
         in/1, out/1, gc_start/1, gc_end/1]).

suite() -> [{ct_hooks,[ts_install_cth]},
            {timetrap, {minutes, 1}}].

all() ->
    [load, unload, reload, invalid_tracers, {group, basic}].

groups() ->
    [{ basic, [], [send, recv, spawn, exit, link, unlink, getting_linked,
                   getting_unlinked, register, unregister, in, out,
                   gc_start, gc_end]}].

init_per_suite(Config) ->
    erlang:trace_pattern({'_','_','_'}, false, [local]),
    erlang:trace_pattern({'_','_','_'}, false, []),
    purge(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TC, Config) when TC =:= load; TC =:= reload ->

    DataDir = proplists:get_value(data_dir, Config),

    Pid = erlang:spawn(fun F() ->
                               receive
                                   {get, Pid} ->
                                       Pid ! DataDir,
                                       F()
                               end
                       end),
    register(tracer_test_config, Pid),
    Config;
init_per_testcase(_, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    case catch tracer_test:enabled(trace_status, self(), self()) of
        discard ->
            ok;
        _ ->
            tracer_test:load(DataDir)
    end,
    Config.

end_per_testcase(TC, _Config) when TC =:= load; TC =:= reload ->
    purge(),
    exit(whereis(tracer_test_config), kill),
    ok;
end_per_testcase(_, _Config) ->
    purge(),
    ok.

load(_Config) ->
    purge(),
    1 = erlang:trace(self(), true, [{tracer, tracer_test, []}, call]),
    purge(),
    1 = erlang:trace_pattern({?MODULE, all, 0}, [],
                             [{meta, tracer_test, []}]),
    ok.

unload(_Config) ->

    ServerFun = fun F(0, undefined) ->
                        receive
                            {N, Pid} -> F(N, Pid)
                        end;
                    F(0, Pid) ->
                        Pid ! done,
                        F(0, undefined);
                    F(N, Pid) ->
                        ?MODULE:all(),
                        F(N-1, Pid)
                end,

    Pid = erlang:spawn_link(fun() -> ServerFun(0, undefined) end),


    Tc = fun(N) ->
                 Pid ! {N, self()},
                 receive done -> ok after 1000 -> ct:fail(timeout) end,
                 trace_delivered(Pid)
         end,

    1 = erlang:trace(Pid, true, [{tracer, tracer_test,
                                  {#{ call => trace }, self(), []}},
                                 call]),
    1 = erlang:trace_pattern({?MODULE, all, 0}, [], []),

    Tc(1),
    receive _M -> ok after 0 -> ct:fail(timeout) end,
    receive M0 -> ct:fail({unexpected_message0, M0}) after 0 -> ok end,

    code:purge(tracer_test),
    code:delete(tracer_test),

    Tc(1),
    receive M1 -> ct:fail({unexpected_message1, M1}) after 0 -> ok end,

    code:purge(tracer_test),

    Tc(1),
    receive M2 -> ct:fail({unexpected_message2, M2}) after 0 -> ok end,

    ok.

%% This testcase is here to make sure there are not
%% segfaults when reloading the current nifs.
reload(_Config) ->

    Tracer = spawn_opt(fun F() -> receive _M -> F() end end,
                       [{message_queue_data, off_heap}]),
    erlang:link(Tracer),
    Tracee = spawn_link(fun reload_loop/0),

    [begin
         Ref = make_ref(),
         State = {#{ call => trace }, Tracer, [Ref]},
         erlang:trace(Tracee, true, [{tracer, tracer_test,State}, call]),
         erlang:trace_pattern({?MODULE, all, 0}, []),

         false = code:purge(tracer_test),
         {module, _} = code:load_file(tracer_test),

         %% There is a race involved in between when the internal nif cache
         %% is purged and when the reload_loop needs the tracer module
         %% so the tracer may be removed or still there.
         case erlang:trace_info(Tracee, tracer) of
             {tracer, []} -> ok;
             {tracer, {tracer_test, State}} -> ok
         end,

         false = code:purge(tracer_test),
         true = code:delete(tracer_test),
         false = code:purge(tracer_test),
         timer:sleep(10)
     end || _ <- lists:seq(1,15)],

    ok.

reload_loop() ->
    ?MODULE:all(),
    ?MODULE:all(),
    ?MODULE:all(),
    ?MODULE:all(),
    ?MODULE:all(),
    timer:sleep(1),
    reload_loop().

invalid_tracers(_Config) ->
    FailTrace = fun(A) ->
                        try erlang:trace(self(), true, A) of
                            _ -> ct:fail(A)
                        catch _:_ -> ok end
                end,

    FailTrace([{tracer, foobar}, call]),
    FailTrace([{tracer, foobar, []}, call]),
    FailTrace([{tracer, make_ref(), []}, call]),
    FailTrace([{tracer, lists, []}, call]),

    FailTP = fun(MS,FL) ->
                     try erlang:trace_pattern({?MODULE,all,0}, MS, FL) of
                            _ -> ct:fail({MS, FL})
                        catch _:_ -> ok end
                end,

    FailTP([],[{meta, foobar}]),
    FailTP([],[{meta, foobar, []}]),
    FailTP([],[{meta, make_ref(), []}]),
    FailTP([],[{meta, lists, []}]),

    ok.



send(_Config) ->

    Self = self(),
    Tc = fun(Pid) ->
                 Pid ! fun() -> Self ! ok end,
                 receive ok -> ok after 100 -> ct:fail(timeout) end
         end,

    Expect = fun(Pid, State, EOpts) ->
                     receive
                         Msg ->
                             {send, State, Pid, ok, Self, Opts} = Msg,
                             check_opts(EOpts, Opts)
                     end
             end,
    test(send, Tc, Expect).


recv(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! ok
         end,

    Expect = fun(Pid, State, EOpts) ->
                     receive
                         Msg ->
                             {'receive', State, Pid, ok, undefined, Opts} = Msg,
                             check_opts(EOpts, Opts)
                     end
             end,

    test('receive', Tc, Expect, false).

spawn(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() -> erlang:spawn(lists,seq,[1,10]), ok end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {spawn, State, Pid, NewPid,
                         {lists,seq,[1,10]}, Opts} = Msg,
                        check_opts(EOpts, Opts),
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(spawn, procs, Tc, Expect, false).

exit(_Config) ->
    Tc = fun(Pid) ->
                 Pid ! fun() -> exit end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {exit, State, Pid, normal, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts)
                end
             end,

    test(exit, procs, Tc, Expect, true, true).

link(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               SPid = erlang:spawn(fun() -> receive _ -> ok end end),
                               erlang:link(SPid),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {link, State, Pid, NewPid, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts),
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(link, procs, Tc, Expect, false).

unlink(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               SPid = erlang:spawn(fun() -> receive _ -> ok end end),
                               erlang:link(SPid),
                               erlang:unlink(SPid),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {unlink, State, Pid, NewPid, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts),
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(unlink, procs, Tc, Expect, false).

getting_linked(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               Self = self(),
                               erlang:spawn(fun() -> erlang:link(Self) end),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {getting_linked, State, Pid, NewPid, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts),
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(getting_linked, procs, Tc, Expect, false).

getting_unlinked(_Config) ->
    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               Self = self(),
                               erlang:spawn(fun() ->
                                                    erlang:link(Self),
                                                    erlang:unlink(Self)
                                            end),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {getting_unlinked, State, Pid, NewPid, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts),
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(getting_unlinked, procs, Tc, Expect, false).

register(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:register(?MODULE, self()),
                               erlang:unregister(?MODULE),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {register, State, Pid, ?MODULE, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts)
                end
             end,

    test(register, procs, Tc, Expect, false).

unregister(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:register(?MODULE, self()),
                               erlang:unregister(?MODULE),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {unregister, State, Pid, ?MODULE, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts)
                end
             end,

    test(unregister, procs, Tc, Expect, false).

in(_Config) ->

    Tc = fun(Pid) ->
                 Self = self(),
                 Pid ! fun() -> receive after 10 -> Self ! ok end end,
                 receive ok -> ok end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                N = (fun F(N) ->
                             receive
                                 Msg ->
                                     {in, State, Pid, _,
                                      undefined, Opts} = Msg,
                                     check_opts(EOpts, Opts),
                                     F(N+1)
                             after 0 -> N
                             end
                     end)(0),
                true = N > 0
             end,

    test(in, running, Tc, Expect, false).

out(_Config) ->
    Tc = fun(Pid) ->
                 Pid ! fun() -> receive after 10 -> exit end end,
                 Ref = erlang:monitor(process, Pid),
                 receive {'DOWN', Ref, _, _, _} -> ok end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                %% We cannot predict how many out schedules there will be
                N = (fun F(N) ->
                             receive
                                 Msg ->
                                     {out, State, Pid, _,
                                      undefined, Opts} = Msg,
                                     check_opts(EOpts, Opts),
                                     F(N+1)
                             after 0 -> N
                             end
                     end)(0),
                true = N > 0
             end,

    test(out, running, Tc, Expect, false, true).

gc_start(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:garbage_collect(),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {gc_major_start, State, Pid, _, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts)
                end
             end,

    test(gc_major_start, garbage_collection, Tc, Expect, false).

gc_end(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:garbage_collect(),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {gc_major_end, State, Pid, _, undefined, Opts} = Msg,
                        check_opts(EOpts, Opts)
                end
             end,

    test(gc_major_end, garbage_collection, Tc, Expect, false).

test(Event, Tc, Expect) ->
    test(Event, Tc, Expect, false).
test(Event, Tc, Expect, Removes) ->
    test(Event, Event, Tc, Expect, Removes).
test(Event, TraceFlag, Tc, Expect, Removes) ->
    test(Event, TraceFlag, Tc, Expect, Removes, false).
test(Event, TraceFlag, Tc, Expect, _Removes, Dies) ->

    ComplexState = {fun() -> ok end, <<0:(128*8)>>},
    Opts = #{ timestamp => undefined,
              scheduler_id => undefined,
              match_spec_result => true },

    %% Test that trace works
    State1 = {#{ Event => trace }, self(), ComplexState},
    Pid1 = start_tracee(),
    1 = erlang:trace(Pid1, true, [TraceFlag, {tracer, tracer_test, State1}]),
    Tc(Pid1),
    ok = trace_delivered(Pid1),

    Expect(Pid1, State1, Opts),
    receive M11 -> ct:fail({unexpected, M11}) after 0 -> ok end,
    if not Dies ->
            {flags, [TraceFlag]} = erlang:trace_info(Pid1, flags),
            {tracer, {tracer_test, State1}} = erlang:trace_info(Pid1, tracer),
            erlang:trace(Pid1, false, [TraceFlag]);
       true -> ok
    end,

    %% Test that trace works with scheduler id and timestamp
    Pid1T = start_tracee(),
    1 = erlang:trace(Pid1T, true, [TraceFlag, {tracer, tracer_test, State1},
                                   timestamp, scheduler_id]),
    Tc(Pid1T),
    ok = trace_delivered(Pid1T),

    Expect(Pid1T, State1, Opts#{ scheduler_id := number,
                                 timestamp := timestamp}),
    receive M11T -> ct:fail({unexpected, M11T}) after 0 -> ok end,
    if not Dies ->
            {flags, [scheduler_id, TraceFlag, timestamp]}
                = erlang:trace_info(Pid1T, flags),
            {tracer, {tracer_test, State1}} = erlang:trace_info(Pid1T, tracer),
            erlang:trace(Pid1T, false, [TraceFlag]);
       true -> ok
    end,

    %% Test that  discard works
    Pid2 = start_tracee(),
    State2 = {#{ Event => discard }, self(), ComplexState},
    1 = erlang:trace(Pid2, true, [TraceFlag, {tracer, tracer_test, State2}]),
    Tc(Pid2),
    ok = trace_delivered(Pid2),
    receive M2 -> ct:fail({unexpected, M2}) after 0 -> ok end,
    if not Dies ->
            {flags, [TraceFlag]} = erlang:trace_info(Pid2, flags),
            {tracer, {tracer_test, State2}} = erlang:trace_info(Pid2, tracer),
            erlang:trace(Pid2, false, [TraceFlag]);
       true ->
            ok
    end,

    ok.

check_opts(#{ scheduler_id := number } = E, #{ scheduler_id := N } = O)
  when is_integer(N) ->
    E1 = maps:remove(scheduler_id, E),
    O1 = maps:remove(scheduler_id, O),
    if E1 == O1 -> ok;
       true -> ct:fail({invalid_opts, E, O})
    end;
check_opts(Opts, Opts) ->
    ok;
check_opts(E,O) ->
    ct:fail({invalid_opts, E, O}).

start_tracee() ->
    spawn_link(
      fun F() ->
              receive
                  Action when is_function(Action) ->
                      case Action() of
                          ok ->
                              F();
                          Err ->
                              Err
                      end;
                  _ ->
                      F()
              end
      end).

trace_delivered(Pid) ->
    Ref = erlang:trace_delivered(Pid),
    receive
        {trace_delivered, Pid, Ref} ->
            ok
    after 1000 ->
            timeout
    end.

purge() ->
    %% Make sure module is not loaded
    case erlang:module_loaded(tracer_test) of
        true ->
            code:purge(tracer_test),
            true = code:delete(tracer_test),
            code:purge(tracer_test);
        _ ->
            ok
    end.
