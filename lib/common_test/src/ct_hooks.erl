%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% @doc Common Test Framework test execution control module.
%%%
%%% <p>This module is a proxy for calling and handling common test hooks.</p>

-module(ct_hooks).

%% API Exports
-export([init/1]).
-export([init_tc/3]).
-export([end_tc/5]).
-export([terminate/1]).
-export([on_tc_skip/2]).
-export([on_tc_fail/2]).

%% If you change this, remember to update ct_util:look -> stop clause as well.
-define(config_name, ct_hooks).

%% -------------------------------------------------------------------------
%% API Functions
%% -------------------------------------------------------------------------

%% @doc Called before any suites are started
-spec init(State :: term()) -> ok |
			       {error, Reason :: term()}.
init(Opts) ->
    call([{Hook, call_id, undefined} || Hook <- get_new_hooks(Opts)],
	 ok, init, []).
		      

%% @doc Called after all suites are done.
-spec terminate(Hooks :: term()) ->
    ok.
terminate(Hooks) ->
    call([{HookId, fun call_terminate/3} || {HookId,_,_} <- Hooks],
	 ct_hooks_terminate_dummy, terminate, Hooks),
    ok.

%% @doc Called as each test case is started. This includes all configuration
%% tests.
-spec init_tc(Mod :: atom(), Func :: atom(), Args :: list()) ->
    NewConfig :: proplists:proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {fail, Reason :: term()}.
init_tc(ct_framework, _Func, Args) ->
    Args;
init_tc(Mod, init_per_suite, Config) ->
    Info = try proplists:get_value(ct_hooks, Mod:suite(),[]) of
	       List when is_list(List) -> 
		   [{ct_hooks,List}];
	       CTHook when is_atom(CTHook) ->
		   [{ct_hooks,[CTHook]}]
	   catch error:undef ->
		   [{ct_hooks,[]}]
	   end,
    call(fun call_generic/3, Config ++ Info, [pre_init_per_suite, Mod]);
init_tc(Mod, end_per_suite, Config) ->
    call(fun call_generic/3, Config, [pre_end_per_suite, Mod]);
init_tc(Mod, {init_per_group, GroupName, Opts}, Config) ->
    maybe_start_locker(Mod, GroupName, Opts),
    call(fun call_generic/3, Config, [pre_init_per_group, GroupName]);
init_tc(_Mod, {end_per_group, GroupName, _}, Config) ->
    call(fun call_generic/3, Config, [pre_end_per_group, GroupName]);
init_tc(_Mod, TC, Config) ->
    call(fun call_generic/3, Config, [pre_init_per_testcase, TC]).

%% @doc Called as each test case is completed. This includes all configuration
%% tests.
-spec end_tc(Mod :: atom(),
	     Func :: atom(),
	     Args :: list(),
	     Result :: term(),
	     Resturn :: term()) ->
    NewConfig :: proplists:proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {fail, Reason :: term()} |
    ok | '$ct_no_change'.
end_tc(ct_framework, _Func, _Args, Result, _Return) ->
    Result;

end_tc(Mod, init_per_suite, Config, _Result, Return) ->
    call(fun call_generic/3, Return, [post_init_per_suite, Mod, Config],
	 '$ct_no_change');

end_tc(Mod, end_per_suite, Config, Result, _Return) ->
    call(fun call_generic/3, Result, [post_end_per_suite, Mod, Config],
	'$ct_no_change');

end_tc(_Mod, {init_per_group, GroupName, _}, Config, _Result, Return) ->
    call(fun call_generic/3, Return, [post_init_per_group, GroupName, Config],
	 '$ct_no_change');

end_tc(Mod, {end_per_group, GroupName, Opts}, Config, Result, _Return) ->
    Res = call(fun call_generic/3, Result,
	       [post_end_per_group, GroupName, Config], '$ct_no_change'),
    maybe_stop_locker(Mod, GroupName,Opts),
    Res;

end_tc(_Mod, TC, Config, Result, _Return) ->
    call(fun call_generic/3, Result, [post_end_per_testcase, TC, Config],
	'$ct_no_change').

on_tc_skip(How, {Suite, Case, Reason}) ->
    call(fun call_cleanup/3, {How, Reason}, [on_tc_skip, Suite, Case]).

on_tc_fail(_How, {Suite, Case, Reason}) ->
    call(fun call_cleanup/3, Reason, [on_tc_fail, Suite, Case]).

%% -------------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------------
call_id(Mod, Config, Meta) when is_atom(Mod) ->
    call_id({Mod, []}, Config, Meta);
call_id({Mod, Opts}, Config, Scope) ->
    Id = catch_apply(Mod,id,[Opts], make_ref()),
    {Config, {Id, scope(Scope), {Mod, {Id,Opts}}}}.
	
call_init({Mod,{Id,Opts}},Config,_Meta) ->
    NewState = Mod:init(Id, Opts),
    {Config, {Mod, NewState}}.

call_terminate({Mod, State}, _, _) ->
    catch_apply(Mod,terminate,[State], ok),
    {[],{Mod,State}}.

call_cleanup({Mod, State}, Reason, [Function, _Suite | Args]) ->
    NewState = catch_apply(Mod,Function, Args ++ [Reason, State],
			   State),
    {Reason, {Mod, NewState}}.

call_generic({Mod, State}, Value, [Function | Args]) ->
    {NewValue, NewState} = catch_apply(Mod, Function, Args ++ [Value, State],
				       {Value,State}),
    {NewValue, {Mod, NewState}}.

%% Generic call function
call(Fun, Config, Meta) ->
    maybe_lock(),
    Hooks = get_hooks(),
    Res = call([{HookId,Fun} || {HookId,_, _} <- Hooks] ++
		   get_new_hooks(Config, Fun),
	       remove(?config_name,Config), Meta, Hooks),
    maybe_unlock(),
    Res.

call(Fun, Config, Meta, NoChangeRet) when is_function(Fun) ->
    case call(Fun,Config,Meta) of
	Config -> NoChangeRet;
	NewReturn -> NewReturn
    end;

call([{Hook, call_id, NextFun} | Rest], Config, Meta, Hooks) ->
    try
	{Config, {NewId, _, _} = NewHook} = call_id(Hook, Config, Meta),
	{NewHooks, NewRest} = 
	    case lists:keyfind(NewId, 1, Hooks) of
		false when NextFun =:= undefined ->
		    {Hooks ++ [NewHook],
		     [{NewId, fun call_init/3} | Rest]};
		ExistingHook when is_tuple(ExistingHook) ->
		    {Hooks, Rest};
		_ ->
		    {Hooks ++ [NewHook],
		     [{NewId, fun call_init/3},{NewId,NextFun} | Rest]}
	    end,
	call(NewRest, Config, Meta, NewHooks)
    catch Error:Reason ->
	    Trace = erlang:get_stacktrace(),
	    ct_logs:log("Suite Hook","Failed to start a CTH: ~p:~p",
			[Error,{Reason,Trace}]),
	    call([], {fail,"Failed to start CTH"
		      ", see the CT Log for details"}, Meta, Hooks)
    end;
call([{HookId, Fun} | Rest], Config, Meta, Hooks) ->
    try
        {_,Scope,ModState} = lists:keyfind(HookId, 1, Hooks),
        {NewConf, NewHookInfo} =  Fun(ModState, Config, Meta),
        NewCalls = get_new_hooks(NewConf, Fun),
        NewHooks = lists:keyreplace(HookId, 1, Hooks, {HookId, Scope, NewHookInfo}),
        call(NewCalls  ++ Rest, remove(?config_name, NewConf), Meta,
             terminate_if_scope_ends(HookId, Meta, NewHooks))
    catch throw:{error_in_cth_call,Reason} ->
            call(Rest, {fail, Reason}, Meta,
                 terminate_if_scope_ends(HookId, Meta, Hooks))
    end;
call([], Config, _Meta, Hooks) ->
    save_suite_data_async(Hooks),
    Config.

remove(Key,List) when is_list(List) ->
    [Conf || Conf <- List, is_tuple(Conf) =:= false
		 orelse element(1, Conf) =/= Key];
remove(_, Else) ->
    Else.

%% Translate scopes, i.e. init_per_group,group1 -> end_per_group,group1 etc
scope([pre_init_per_testcase, TC|_]) ->
    [post_end_per_testcase, TC];
scope([pre_init_per_group, GroupName|_]) ->
    [post_end_per_group, GroupName];
scope([post_init_per_group, GroupName|_]) ->
    [post_end_per_group, GroupName];
scope([pre_init_per_suite, SuiteName|_]) ->
    [post_end_per_suite, SuiteName];
scope([post_init_per_suite, SuiteName|_]) ->
    [post_end_per_suite, SuiteName];
scope(init) ->
    none.

terminate_if_scope_ends(HookId, [on_tc_skip,_Suite,{end_per_group,Name}], 
			Hooks) ->
    terminate_if_scope_ends(HookId, [post_end_per_group, Name], Hooks);
terminate_if_scope_ends(HookId, [on_tc_skip,Suite,end_per_suite], Hooks) ->
    terminate_if_scope_ends(HookId, [post_end_per_suite, Suite], Hooks);
terminate_if_scope_ends(HookId, [Function,Tag|T], Hooks) when T =/= [] ->
    terminate_if_scope_ends(HookId,[Function,Tag],Hooks);
terminate_if_scope_ends(HookId, Function, Hooks) ->
    case lists:keyfind(HookId, 1, Hooks) of
        {HookId, Function, _ModState} = Hook ->
            terminate([Hook]),
            lists:keydelete(HookId, 1, Hooks);
        _ ->
            Hooks
    end.

%% Fetch hook functions
get_new_hooks(Config, Fun) ->
    lists:foldl(fun(NewHook, Acc) ->
			[{NewHook, call_id, Fun} | Acc]
		end, [], get_new_hooks(Config)).

get_new_hooks(Config) when is_list(Config) ->
    lists:flatmap(fun({?config_name, HookConfigs}) ->
			  HookConfigs;
		     (_) ->
			  []
		  end, Config);
get_new_hooks(_Config) ->
    [].

save_suite_data_async(Hooks) ->
    ct_util:save_suite_data_async(?config_name, Hooks).

get_hooks() ->
    ct_util:read_suite_data(?config_name).

catch_apply(M,F,A, Default) ->
    try
	apply(M,F,A)
    catch error:Reason ->
	    case erlang:get_stacktrace() of
            %% Return the default if it was the CTH module which did not have the function.
		[{M,F,A}|_] when Reason == undef ->
		    Default;
		Trace ->
		    ct_logs:log("Suite Hook","Call to CTH failed: ~p:~p",
				[error,{Reason,Trace}]),
		    throw({error_in_cth_call,
			   lists:flatten(
			     io_lib:format("~p:~p/~p CTH call failed",
					   [M,F,length(A)]))})
	    end
    end.


%% We need to lock around the state for parallel groups only. This is because
%% we will get several processes reading and writing the state for a single
%% cth at the same time.
maybe_start_locker(Mod,GroupName,Opts) ->
    case lists:member(parallel,Opts) of
	true ->
	    {ok, _Pid} = ct_hooks_lock:start({Mod,GroupName});
	false ->
	    ok
    end.

maybe_stop_locker(Mod,GroupName,Opts) ->
    case lists:member(parallel,Opts) of
	true ->
	    stopped = ct_hooks_lock:stop({Mod,GroupName});
	false ->
	    ok
    end.


maybe_lock() ->
    locked = ct_hooks_lock:request().

maybe_unlock() ->
    unlocked = ct_hooks_lock:release().
