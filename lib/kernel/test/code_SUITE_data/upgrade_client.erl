-module(upgrade_client).

-export([run/5]).

%%-define(line, io:format("~s:~p\n", [?MODULE,?LINE]),).
-define(line,).    

run(Dir, Upgradee1, Upgradee2, Other1, Other2) ->
    %% Load version 1 of upgradee
    code_SUITE:compile_load(upgradee, Dir, 1, Upgradee1),

    Tracer = start_tracing(),

    ?line 1 = upgradee:exp1(),
    ?line 1 = upgradee:exp1exp2(),
    ?line 1 = upgradee:exp1loc2(),

    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1exp2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1loc2()),

    P = spawn_link(upgradee,dispatch_loop,[]),

    ?line 1 = proxy_call(P, local, exp1),
    ?line 1 = proxy_call(P, local, loc1),
    ?line 1 = proxy_call(P, local, exp1exp2),
    ?line 1 = proxy_call(P, local, exp1loc2),
    ?line 1 = proxy_call(P, local, loc1exp2),
    ?line 1 = proxy_call(P, local, loc1loc2),
    ?line 1 = proxy_call(P, external, exp1),
    ?line 1 = proxy_call(P, external, exp1exp2),
    ?line 1 = proxy_call(P, external, exp1loc2),

    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc2),
    ?line {cannot_compile,1} = proxy_call(P, local, exp2),
    ?line {cannot_compile,1} = proxy_call(P, local, loc2),

    ?line {'EXIT',{undef,_}} = (catch other:exp1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp1exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc11exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc2),

    Env1 = "Env1",
    put(loc1_fun, upgradee:get_local_fun(Env1)),
    ?line {1,Env1} = (get(loc1_fun))(),

    put(exp1exp2_fun, upgradee:get_exp1exp2_fun()),
    ?line 1 = (get(exp1exp2_fun))(),

    ok = check_tracing(Tracer, 13),

    %%
    %% Load version 1 of other
    %%
    code_SUITE:compile_load(other, Dir, 1, Other1),
    ?line 1 = other:exp1(),
    ?line 1 = other:exp1loc2(),
    ?line 1 = other:exp1exp2(),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),

    ?line 1 = proxy_call(P, other, exp1),
    ?line 1 = proxy_call(P, other, exp1loc2),
    ?line 1 = proxy_call(P, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc2),

    ok = check_tracing(Tracer, 5),

    %%
    %% Load version 2 of upgradee
    %%
    code_SUITE:compile_load(upgradee, Dir, 2, Upgradee2),

    ?line 2 = upgradee:exp2(),
    ?line 2 = upgradee:exp1exp2(),
    ?line 2 = upgradee:loc1exp2(),
    
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc2()),

    ?line 1 = proxy_call(P, local, exp1),
    ?line 1 = proxy_call(P, local, loc1),
    ?line 1 = proxy_call(P, local, exp1exp2),
    ?line 1 = proxy_call(P, local, exp1loc2),
    ?line 1 = proxy_call(P, local, loc1exp2),
    ?line 1 = proxy_call(P, local, loc1loc2),
    ?line {cannot_compile,1} = proxy_call(P, local, exp2),
    ?line {cannot_compile,1} = proxy_call(P, local, loc2),

    ?line 2 = proxy_call(P, external, exp1exp2),
    ?line 2 = proxy_call(P, external, loc1exp2),
    ?line 2 = proxy_call(P, external, exp2),

    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp1),    
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc2),

    ?line 1 = other:exp1(),
    ?line 1 = other:exp1loc2(),
    ?line 1 = other:exp1exp2(),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),

    ?line 1 = proxy_call(P, other, exp1),
    ?line 1 = proxy_call(P, other, exp1loc2),
    ?line 1 = proxy_call(P, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc2),

    ?line {1,Env1} = (get(loc1_fun))(),
    Env2 = "Env2",
    put(loc2_fun, upgradee:get_local_fun(Env2)),
    ?line {2,Env2} = (get(loc2_fun))(),

    ?line 2 = (get(exp1exp2_fun))(),

    ok = check_tracing(Tracer, 10),

    %%
    %% Load version 2 of other
    %%
    code_SUITE:compile_load(other, Dir, 2, Other2),

    ?line 2 = upgradee:exp2(),
    ?line 2 = upgradee:exp1exp2(),
    ?line 2 = upgradee:loc1exp2(),
    
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc2()),

    ?line 1 = proxy_call(P, local, exp1),
    ?line 1 = proxy_call(P, local, loc1),
    ?line 1 = proxy_call(P, local, exp1exp2),
    ?line 1 = proxy_call(P, local, exp1loc2),
    ?line 1 = proxy_call(P, local, loc1exp2),
    ?line 1 = proxy_call(P, local, loc1loc2),
    ?line {cannot_compile,1} = proxy_call(P, local, exp2),
    ?line {cannot_compile,1} = proxy_call(P, local, loc2),

    ?line 2 = proxy_call(P, external, exp1exp2),
    ?line 2 = proxy_call(P, external, loc1exp2),
    ?line 2 = proxy_call(P, external, exp2),

    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp1),    
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc2),

    ?line 2 = other:exp2(),
    ?line 2 = other:loc1exp2(),
    ?line 2 = other:exp1exp2(),
    ?line {'EXIT',{undef,_}} = (catch other:exp1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),

    ?line 2 = proxy_call(P, other, exp2),
    ?line 2 = proxy_call(P, other, loc1exp2),
    ?line 2 = proxy_call(P, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc2),

    ?line {1,Env1} = (get(loc1_fun))(),
    ?line {2,Env2} = (get(loc2_fun))(),
    ?line 2 = (get(exp1exp2_fun))(),

    ok = check_tracing(Tracer, 10),

    %%
    %% Upgrade proxy to version 2
    %%
    P ! upgrade_order,

    %%
    io:format("Purge version 1 of 'upgradee'\n",[]),
    %%
    put(loc1_fun,undefined),
    code:purge(upgradee),

    %%
    io:format("Delete version 2 of 'upgradee'\n",[]),
    %%
    code:delete(upgradee),
    
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1exp2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1exp2()),    
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc2()),
    
    ?line 2 = proxy_call(P, local, exp2),
    ?line 2 = proxy_call(P, local, loc2),
    ?line 2 = proxy_call(P, local, exp1exp2),
    ?line 2 = proxy_call(P, local, exp1loc2),
    ?line 2 = proxy_call(P, local, loc1exp2),
    ?line 2 = proxy_call(P, local, loc1loc2),
    ?line {cannot_compile,2} = proxy_call(P, local, exp1),
    ?line {cannot_compile,2} = proxy_call(P, local, loc1),
    
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp1),    
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, external, loc2),
    
    ?line 2 = other:exp2(),
    ?line 2 = other:loc1exp2(),
    ?line 2 = other:exp1exp2(),
    ?line {'EXIT',{undef,_}} = (catch other:exp1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),
    
    ?line 2 = proxy_call(P, other, exp2),
    ?line 2 = proxy_call(P, other, loc1exp2),
    ?line 2 = proxy_call(P, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P, other, loc2),

    ?line {'EXIT',{undef,_}} = (catch (get(exp1exp2_fun))()),
    
    ok = check_tracing(Tracer, 14),

    unlink(P),
    exit(P, die_please),

    io:format("Purge 'upgradee'\n",[]),
    put(loc2_fun,undefined),
    code:purge(upgradee),

    io:format("Delete and purge 'other'\n",[]),
    code:purge(other),
    code:delete(other),
    code:purge(other),

    stop_tracing(Tracer),
    ok.

proxy_call(Pid, CallType, Func) ->
    Pid ! {self(), CallType, Func},
    receive
	{Pid, call_result, Func, Ret} -> Ret
    end.


start_tracing() ->
    Self = self(),
    {Tracer,_} = spawn_opt(fun() -> tracer_loop(Self) end, [link,monitor]),
    ?line 1 = erlang:trace_pattern({error_handler,undefined_function,3},
				   true, [global]),
    ?line 1 = erlang:trace(Self, true, [call,{tracer,Tracer}]),
    Tracer.


tracer_loop(Receiver) ->
    receive
	die_please ->
	    ok;
	{do_trace_delivered, Tracee} ->
	    _ = erlang:trace_delivered(Tracee),
	    tracer_loop(Receiver);

	Msg ->
	    Receiver ! Msg,
	    tracer_loop(Receiver)
    end.

check_tracing(Tracer, Expected) ->
    Tracer ! {do_trace_delivered, self()},
    case check_tracing_loop(0,[]) of
        {Expected,_} ->
            ok;
        {Got, MsgList} ->
            io:format("Expected ~p trace msg, got ~p:\n~p\n",
                      [Expected, Got, lists:reverse(MsgList)]),
            "Trace msg mismatch"
    end.

check_tracing_loop(N, MsgList) ->
    Self = self(),
    receive
	{trace, _Pid, call, {_M, _F, _Args}} = Msg ->
	    check_tracing_loop(N+1, [Msg | MsgList]);
	{trace_delivered, Self, _} ->
	    {N, MsgList}
    end.


stop_tracing(Tracer) ->
    erlang:trace_pattern({error_handler,undefined_function,3}, false, [global]),
    erlang:trace(self(), false, [call]),
    Tracer ! die_please,
    receive
	{'DOWN', _, process, Tracer, _} -> ok
    end.
