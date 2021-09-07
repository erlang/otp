-module(call_purged_fun_tester).

-export([do/4]).

-define(line,).
do(P,D,T,O) ->
    do_it(P,D,T,O).

do_it(Priv, Data, Type, Opts) ->
    File = filename:join(Data, "my_code_test2"),
    Code = filename:join(Priv, "my_code_test2"),

    catch erlang:purge_module(my_code_test2),
    catch erlang:delete_module(my_code_test2),
    catch erlang:purge_module(my_code_test2),

    {ok,my_code_test2} = c:c(File, [{outdir,Priv} | Opts]),

    T = ets:new(my_code_test2_fun_table, []),
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

    PurgeType = case Type of
		    code_gone ->
			ok = file:delete(Code++".beam"),
			true;
		    code_reload ->
			true;
		    code_there ->
			false
		end,

    true = erlang:delete_module(my_code_test2),

    ok = receive {P0, "going to sleep"} -> ok
	 after 1000 -> timeout
	 end,

    Purge = start_purge(my_code_test2, PurgeType),

    {P1, M1} = spawn_monitor(fun () ->
                                     [{my_fun,F}] = ets:lookup(T, my_fun),
                                     4712 = F(1),
                                     exit(completed)
                             end),

    ok =  wait_until(fun () ->
			     {status, suspended}
				 == process_info(P1, status)
		     end),

    ok = continue_purge(Purge),

    {P2, M2} = spawn_monitor(fun () ->
                                     [{my_fun,F}] = ets:lookup(T, my_fun),
                                     4713 = F(2),
                                     exit(completed)
			     end),
    {P3, M3} = spawn_monitor(fun () ->
				     [{my_fun,F}] = ets:lookup(T, my_fun),
				     4714 = F(3),
				     exit(completed)
			     end),

    ok = wait_until(fun () ->
			    {status, suspended}
				== process_info(P2, status)
		    end),
    ok = wait_until(fun () ->
			    {status, suspended}
				== process_info(P3, status)
		    end),

    {current_function,
     {erts_code_purger,
      pending_purge_lambda,
      3}} = process_info(P1, current_function),
    {current_function,
     {erts_code_purger,
      pending_purge_lambda,
      3}} = process_info(P2, current_function),
    {current_function,
     {erts_code_purger,
      pending_purge_lambda,
      3}} = process_info(P3, current_function),

    case Type of
	code_there ->
	    false = complete_purge(Purge),
            P0 ! {self(), "wake up"},
            completed = wait_for_down(P0,M0);
	_ ->
	    {true, true} = complete_purge(Purge),
            killed = wait_for_down(P0,M0)
    end,

    case Type of
	code_gone ->
            {undef, _} = wait_for_down(P1,M1),
            {undef, _} = wait_for_down(P2,M2),
            {undef, _} = wait_for_down(P3,M3);
	_ ->
            completed = wait_for_down(P1,M1),
            completed = wait_for_down(P2,M2),
            completed = wait_for_down(P3,M3),
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
