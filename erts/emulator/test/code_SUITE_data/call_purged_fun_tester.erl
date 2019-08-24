-module(call_purged_fun_tester).

-export([do/4]).

%% Resurrect line macro when hipe compiled
-ifdef(hipe).
-define(line, put(the_line,?LINE),).
do(Priv, Data, Type, Opts) ->
    try do_it(Priv, Data, Type, Opts)
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            io:format("Caught exception from line ~p:\n~p\n",
                      [get(the_line), ST]),
            io:format("Message queue: ~p\n", [process_info(self(), messages)]),
            erlang:raise(C, E, ST)
    end.
-else.
-define(line,).
do(P,D,T,O) ->
    do_it(P,D,T,O).
-endif.


do_it(Priv, Data, Type, Opts) ->
    File = filename:join(Data, "my_code_test2"),
    Code = filename:join(Priv, "my_code_test2"),

    catch erlang:purge_module(my_code_test2),
    catch erlang:delete_module(my_code_test2),
    catch erlang:purge_module(my_code_test2),

    ?line {ok,my_code_test2} = c:c(File, [{outdir,Priv} | Opts]),

    ?line IsNative = lists:member(native,Opts),
    ?line IsNative = code:is_module_native(my_code_test2),

    ?line T = ets:new(my_code_test2_fun_table, []),
    ets:insert(T, {my_fun,my_code_test2:make_fun(4711)}),
    ets:insert(T, {my_fun2,my_code_test2:make_fun2()}),

    Papa = self(),
    {P0,M0} = spawn_monitor(fun () ->
                                    [{my_fun2,F2}] = ets:lookup(T, my_fun2),
                                    F2(fun () ->
                                               Papa ! {self(),"going to sleep"},
                                               receive {Papa,"wake up"} -> ok end
                                       end,
                                       fun () -> ok end),
                                    exit(completed)
                            end),

    ?line PurgeType = case Type of
		    code_gone ->
			ok = file:delete(Code++".beam"),
			true;
		    code_reload ->
			true;
		    code_there ->
			false
		end,

    ?line true = erlang:delete_module(my_code_test2),

    ?line ok = receive {P0, "going to sleep"} -> ok
               after 1000 -> timeout
               end,

    ?line Purge = start_purge(my_code_test2, PurgeType),

    ?line {P1, M1} = spawn_monitor(fun () ->
                                           ?line [{my_fun,F}] = ets:lookup(T, my_fun),
                                           ?line 4712 = F(1),
                                           exit(completed)
			     end),

   ?line ok =  wait_until(fun () ->
                                  {status, suspended}
                                      == process_info(P1, status)
                          end),

    ?line ok = continue_purge(Purge),

    ?line {P2, M2} = spawn_monitor(fun () ->
                                           ?line [{my_fun,F}] = ets:lookup(T, my_fun),
                                           ?line 4713 = F(2),
                                           exit(completed)
			     end),
    ?line {P3, M3} = spawn_monitor(fun () ->
                                           ?line [{my_fun,F}] = ets:lookup(T, my_fun),
                                           ?line 4714 = F(3),
                                           exit(completed)
			     end),

    ?line ok = wait_until(fun () ->
                                  {status, suspended}
                                      == process_info(P2, status)
                          end),
    ?line ok = wait_until(fun () ->
                                  {status, suspended}
                                      == process_info(P3, status)
                          end),

    ?line {current_function,
     {erts_code_purger,
      pending_purge_lambda,
      3}} = process_info(P1, current_function),
    ?line {current_function,
     {erts_code_purger,
      pending_purge_lambda,
      3}} = process_info(P2, current_function),
    ?line {current_function,
     {erts_code_purger,
      pending_purge_lambda,
      3}} = process_info(P3, current_function),

    case Type of
	code_there ->
	    ?line false = complete_purge(Purge),
            P0 ! {self(), "wake up"},
            ?line completed = wait_for_down(P0,M0);
	_ ->
	    ?line {true, true} = complete_purge(Purge),
            ?line killed = wait_for_down(P0,M0)
    end,

    case Type of
	code_gone ->
            ?line {undef, _} = wait_for_down(P1,M1),
            ?line {undef, _} = wait_for_down(P2,M2),
            ?line {undef, _} = wait_for_down(P3,M3);
	_ ->
            ?line completed = wait_for_down(P1,M1),
            ?line completed = wait_for_down(P2,M2),
            ?line completed = wait_for_down(P3,M3),
	    catch erlang:purge_module(my_code_test2),
	    catch erlang:delete_module(my_code_test2),
	    catch erlang:purge_module(my_code_test2)
    end,
    ok.

wait_for_down(P,M) ->
    receive
        {'DOWN', M, process, P, Reason} ->
            Reason
    after 1000 ->
            timeout
    end.

wait_until(Fun) ->
    wait_until(Fun, 20).

wait_until(Fun, N) ->
    case {Fun(),N} of
	{true, _} ->
	    ok;
        {false, 0} ->
            timeout;
	{false, _} ->
	    receive after 100 -> ok end,
	    wait_until(Fun, N-1)
    end.

start_purge(Mod, Type) when is_atom(Mod)
			    andalso ((Type == true)
				     orelse (Type == false)) ->
    Ref = make_ref(),
    erts_code_purger ! {test_purge, Mod, self(), Type, Ref},
    receive
	{started, Ref} ->
	    Ref
    end.

continue_purge(Ref) when is_reference(Ref) ->
    erts_code_purger ! {continue, Ref},
    receive
	{continued, Ref} ->
	    ok
    end.

complete_purge(Ref) when is_reference(Ref) ->
    erts_code_purger ! {complete, Ref},
    receive
	{test_purge, Res, Ref} ->
	    Res
    end.
