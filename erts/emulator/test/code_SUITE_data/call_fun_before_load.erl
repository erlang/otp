-module(call_fun_before_load).
-export([run/2]).

run(ObjFile, Code) ->
    Ref = make_ref(),
    Self = self(),
    {Pid, MRef} =
        spawn_monitor(
          fun() ->
                  %% If we're called before being loaded, process_info/2 can't
                  %% figure out where we are, reporting that we're in an
                  %% unknown module rather than the current one.
                  Fun0 = fun(Fun) ->
                                 {current_function,
                                  {?MODULE, _, _}} =
                                     process_info(self(), current_function),
                                 Fun(Fun)
                         end,

                  NumScheds = erlang:system_info(schedulers_online),
                  Workers = [spawn_link(fun() -> Fun0(Fun0) end)
                             || _ <- lists:seq(1, NumScheds)],

                  %% Give the workers time to start before reloading ourselves.
                  timer:sleep(100),

                  code:load_binary(?MODULE, ObjFile, Code),

                  %% Give the workers time to crash before exiting.
                  timer:sleep(100),

                  Self ! {Ref, Workers},

                  ok
          end),
    receive
        {Ref, Workers} ->
            erlang:demonitor(MRef, [flush]),
            [exit(Worker, kill) || Worker <- Workers],
            ok;
        {'DOWN', MRef, process, Pid, {{badmatch,_}, _}} ->
            ct:fail("Ran newly staged but not yet loaded code.", []);
        {'DOWN', MRef, process, Pid, Reason} ->
            ct:fail(Reason)
    end.
