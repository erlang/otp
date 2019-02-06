%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-export([send/1, recv/1, call/1, call_return/1, spawn/1, exit/1,
         link/1, unlink/1, getting_linked/1, getting_unlinked/1,
         register/1, unregister/1, in/1, out/1, gc_start/1, gc_end/1,
         seq_trace/1]).

suite() -> [{ct_hooks,[ts_install_cth]},
            {timetrap, {minutes, 1}}].

all() ->
    [load, unload, reload, invalid_tracers, {group, basic}].

groups() ->
    [{ basic, [], [send, recv, call, call_return, spawn, exit,
                   link, unlink, getting_linked, getting_unlinked,
                   register, unregister, in, out, gc_start, gc_end,
                   seq_trace]}].

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
    common_init_per_testcase(Config);
init_per_testcase(_, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    case catch tracer_test:enabled(trace_status, self(), self()) of
        discard ->
            ok;
        _ ->
            tracer_test:load(DataDir)
    end,
    common_init_per_testcase(Config).

common_init_per_testcase(Config) ->
    Killer = erlang:spawn(fun() -> killer_loop([]) end),
    register(killer_process, Killer),
    Config.

end_per_testcase(TC, _Config) when TC =:= load; TC =:= reload ->
    purge(),
    exit(whereis(tracer_test_config), kill),
    kill_processes();
end_per_testcase(_, _Config) ->
    purge(),
    kill_processes().

kill_processes() ->
    killer_process ! {get_pids,self()},
    receive
        {pids_to_kill,Pids} -> ok
    end,
    _ = [begin
             case erlang:is_process_alive(P) of
                 true ->
                     io:format("Killing ~p\n", [P]);
                 false ->
                     ok
             end,
             erlang:unlink(P),
             exit(P, kill)
         end || P <- Pids],
    ok.

killer_loop(Pids) ->
    receive
        {add_pid,Pid} ->
            killer_loop([Pid|Pids]);
        {get_pids,To} ->
            To ! {pids_to_kill,Pids}
    end.

kill_me(Pid) ->
    killer_process ! {add_pid,Pid},
    Pid.

%%% Test cases follow.

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
                             {send, State, Pid, ok, Opts} = Msg,
                             check_opts(EOpts, Opts, Self)
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
                             {'receive', State, Pid, ok, Opts} = Msg,
                             check_opts(EOpts, Opts)
                     end
             end,

    test('receive', Tc, Expect, false).

call(_Config) ->

    Self = self(),
    Tc = fun(Pid) ->
                 Pid ! fun() -> call_test(Self), Self ! ok end,
                 receive ok -> ok after 100 -> ct:fail(timeout) end
         end,

    erlang:trace_pattern({?MODULE, call_test, 1}, [], [local]),

    Expect = fun(Pid, State, EOpts) ->
                     receive
                         Msg ->
                             {call, State, Pid, {?MODULE, call_test, [Self]}, Opts} = Msg,
                             check_opts(EOpts, Opts)
                     end
             end,
    test(call, Tc, Expect).

call_return(_Config) ->

    Self = self(),
    Tc = fun(Pid) ->
                 Pid ! fun() -> call_test(undefined), Self ! ok end,
                 receive ok -> ok after 100 -> ct:fail(timeout) end
         end,

    1 = erlang:trace_pattern({?MODULE, call_test, 1}, [{'_',[],[{return_trace}]}], [local]),

    Expect = fun(Pid, State, EOpts) ->
                     receive
                         CallMsg ->
                             {call, State, Pid, {?MODULE, call_test, [undefined]}, COpts} = CallMsg,
                             check_opts(EOpts, COpts)
                     end,
                     receive
                         RetMsg ->
                             {return_from, State, Pid, {?MODULE, call_test, 1}, ROpts} = RetMsg,
                             check_opts(EOpts, ROpts, undefined)
                     end
             end,
    test(call, Tc, Expect).

call_test(Arg) ->
    Arg.

spawn(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() -> kill_me(erlang:spawn(lists,seq,[1,10])), ok end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {spawn, State, Pid, NewPid, Opts} = Msg,
                        check_opts(EOpts, Opts, {lists,seq,[1,10]}),
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
                        {exit, State, Pid, normal, Opts} = Msg,
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
                        {link, State, Pid, NewPid, Opts} = Msg,
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
                               kill_me(SPid),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State, EOpts) ->
                receive
                    Msg ->
                        {unlink, State, Pid, NewPid, Opts} = Msg,
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
                        {getting_linked, State, Pid, NewPid, Opts} = Msg,
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
                        {getting_unlinked, State, Pid, NewPid, Opts} = Msg,
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
                        {register, State, Pid, ?MODULE, Opts} = Msg,
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
                        {unregister, State, Pid, ?MODULE, Opts} = Msg,
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
                                     {in, State, Pid, _, Opts} = Msg,
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
                                     {out, State, Pid, _, Opts} = Msg,
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
                        {gc_major_start, State, Pid, _, Opts} = Msg,
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
                        {gc_major_end, State, Pid, _, Opts} = Msg,
                        check_opts(EOpts, Opts)
                end
             end,

    test(gc_major_end, garbage_collection, Tc, Expect, false).

seq_trace(_Config) ->

    seq_trace:set_system_tracer({tracer_test,
                                 {#{ seq_trace => trace }, self(), []}}),
    erlang:spawn(fun() ->
                         seq_trace:set_token(label,17),
                         seq_trace:set_token(print,true),
                         seq_trace:print(17,"**** Trace Started ****")
                 end),
    receive
        {seq_trace, _, 17, {print, _, _, _, _}, _} ->
            ok;
        M ->
            ct:fail("~p~n",[M])
    after 100 ->
            ct:fail(timeout)
    end.

test(Event, Tc, Expect) ->
    test(Event, Tc, Expect, false).
test(Event, Tc, Expect, Removes) ->
    test(Event, Event, Tc, Expect, Removes).
test(Event, TraceFlag, Tc, Expect, Removes) ->
    test(Event, TraceFlag, Tc, Expect, Removes, false).
test(Event, TraceFlag, Tc, Expect, _Removes, Dies) ->

    ComplexState = {fun() -> ok end, <<0:(128*8)>>},
    Opts = #{ },

    %% Test that trace works
    State1 = {#{ Event => trace }, self(), ComplexState},
    Pid1 = start_tracee(),
    1 = erlang:trace(Pid1, true, [TraceFlag, {tracer, tracer_test, State1}]),
    Tc(Pid1),
    ok = trace_delivered(Pid1),

    Expect(Pid1, State1, Opts),
    receive M11 -> ct:fail({unexpected, M11}) after 0 -> ok end,
    if not Dies andalso Event /= in ->
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

    Expect(Pid1T, State1, Opts#{ scheduler_id => number,
                                 timestamp => timestamp}),
    receive M11T -> ct:fail({unexpected, M11T}) after 0 -> ok end,
    if not Dies andalso Event /= in ->
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
    if not Dies andalso Event /= in ->
            {flags, [TraceFlag]} = erlang:trace_info(Pid2, flags),
            {tracer, {tracer_test, State2}} = erlang:trace_info(Pid2, tracer),
            erlang:trace(Pid2, false, [TraceFlag]);
       true ->
            ok
    end,

    ok.

check_opts(E, O, Extra) ->
    check_opts(E#{ extra => Extra }, O).
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
