%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

%% All of the hooks which are to be started by default. Remove by issuing
%% -enable_builtin_hooks false to when starting common test.
-define(BUILTIN_HOOKS,[#ct_hook_config{ module = cth_log_redirect,
					opts = [],
					prio = ctfirst }]).

-record(ct_hook_config, {id, module, prio, scope, opts = [], state = []}).

%% -------------------------------------------------------------------------
%% API Functions
%% -------------------------------------------------------------------------

%% @doc Called before any suites are started
-spec init(State :: term()) -> ok |
			       {fail, Reason :: term()}.
init(Opts) ->
    call(get_builtin_hooks(Opts) ++ get_new_hooks(Opts, undefined),
	 ok, init, []).

%% @doc Called after all suites are done.
-spec terminate(Hooks :: term()) ->
    ok.
terminate(Hooks) ->
    call([{HookId, fun call_terminate/3}
	  || #ct_hook_config{id = HookId} <- Hooks],
	 ct_hooks_terminate_dummy, terminate, Hooks),
    ok.

%% @doc Called as each test case is started. This includes all configuration
%% tests.
-spec init_tc(Mod :: atom(),
	      FuncSpec :: atom() | 
			  {ConfigFunc :: init_per_testcase | end_per_testcase,
			   TestCase :: atom()} |
			  {ConfigFunc :: init_per_group | end_per_group,
			   GroupName :: atom(),
			   Properties :: list()},
	      Args :: list()) ->
    NewConfig :: proplists:proplist() |
		 {skip, Reason :: term()} |
		 {auto_skip, Reason :: term()} |
		 {fail, Reason :: term()}.

init_tc(Mod, init_per_suite, Config) ->
    Info = try proplists:get_value(ct_hooks, Mod:suite(),[]) of
	       List when is_list(List) -> 
		   [{?config_name,List}];
	       CTHook when is_atom(CTHook) ->
		   [{?config_name,[CTHook]}]
	   catch error:undef ->
		   [{?config_name,[]}]
	   end,
    call(fun call_generic/3, Config ++ Info, [pre_init_per_suite, Mod]);
init_tc(Mod, end_per_suite, Config) ->
    call(fun call_generic/3, Config, [pre_end_per_suite, Mod]);
init_tc(Mod, {init_per_group, GroupName, Properties}, Config) ->
    maybe_start_locker(Mod, GroupName, Properties),
    call(fun call_generic_fallback/3, Config,
         [pre_init_per_group, Mod, GroupName]);
init_tc(Mod, {end_per_group, GroupName, _}, Config) ->
    call(fun call_generic_fallback/3, Config,
         [pre_end_per_group, Mod, GroupName]);
init_tc(Mod, {init_per_testcase,TC}, Config) ->
    call(fun call_generic_fallback/3, Config, [pre_init_per_testcase, Mod, TC]);
init_tc(Mod, {end_per_testcase,TC}, Config) ->
    call(fun call_generic_fallback/3, Config, [pre_end_per_testcase, Mod, TC]);
init_tc(Mod, TC = error_in_suite, Config) ->
    call(fun call_generic_fallback/3, Config, [pre_init_per_testcase, Mod, TC]).

%% @doc Called as each test case is completed. This includes all configuration
%% tests.
-spec end_tc(Mod :: atom(),
	     FuncSpec :: atom() |  
			 {ConfigFunc :: init_per_testcase | end_per_testcase,
			  TestCase :: atom()} |
			 {ConfigFunc :: init_per_group | end_per_group,
			  GroupName :: atom(),
			  Properties :: list()},
	     Args :: list(),
	     Result :: term(),
	     Return :: term()) ->
    NewConfig :: proplists:proplist() |
		 {skip, Reason :: term()} |
		 {auto_skip, Reason :: term()} |
		 {fail, Reason :: term()} |
		 ok | '$ct_no_change'.

end_tc(Mod, init_per_suite, Config, _Result, Return) ->
    call(fun call_generic/3, Return, [post_init_per_suite, Mod, Config],
	 '$ct_no_change');
end_tc(Mod, end_per_suite, Config, Result, _Return) ->
    call(fun call_generic/3, Result, [post_end_per_suite, Mod, Config],
	'$ct_no_change');
end_tc(Mod, {init_per_group, GroupName, _}, Config, _Result, Return) ->
    call(fun call_generic_fallback/3, Return,
         [post_init_per_group, Mod, GroupName, Config], '$ct_no_change');
end_tc(Mod, {end_per_group, GroupName, Properties}, Config, Result, _Return) ->
    Res = call(fun call_generic_fallback/3, Result,
	       [post_end_per_group, Mod, GroupName, Config], '$ct_no_change'),
    maybe_stop_locker(Mod, GroupName, Properties),
    Res;
end_tc(Mod, {init_per_testcase,TC}, Config, Result, _Return) ->
    call(fun call_generic_fallback/3, Result,
         [post_init_per_testcase, Mod, TC, Config], '$ct_no_change');
end_tc(Mod, {end_per_testcase,TC}, Config, Result, _Return) ->
    call(fun call_generic_fallback/3, Result,
         [post_end_per_testcase, Mod, TC, Config], '$ct_no_change');
end_tc(Mod, TC = error_in_suite, Config, Result, _Return) ->
    call(fun call_generic_fallback/3, Result,
         [post_end_per_testcase, Mod, TC, Config], '$ct_no_change').


%% Case = TestCase | {TestCase,GroupName}
on_tc_skip(How, {Suite, Case, Reason}) ->
    call(fun call_cleanup/3, {How, Reason}, [on_tc_skip, Suite, Case]).

%% Case = TestCase | {TestCase,GroupName}
on_tc_fail(_How, {Suite, Case, Reason}) ->
    call(fun call_cleanup/3, Reason, [on_tc_fail, Suite, Case]).

%% -------------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------------
call_id(#ct_hook_config{ module = Mod, opts = Opts} = Hook, Config, Scope) ->
    Id = catch_apply(Mod,id,[Opts], make_ref()),
    {Config, Hook#ct_hook_config{ id = Id, scope = scope(Scope)}}.
	
call_init(#ct_hook_config{ module = Mod, opts = Opts, id = Id, prio = P} = Hook,
	  Config,_Meta) ->
    case Mod:init(Id, Opts) of
	{ok, NewState} when P =:= undefined ->
	    {Config, Hook#ct_hook_config{ state = NewState, prio = 0 } };
	{ok, NewState} ->
	    {Config, Hook#ct_hook_config{ state = NewState } };
	{ok, NewState, Prio} when P =:= undefined ->
	    %% Only set prio if not already set when installing hook
	    {Config, Hook#ct_hook_config{ state = NewState, prio = Prio } };
	{ok, NewState, _} ->
	    {Config, Hook#ct_hook_config{ state = NewState } };
	NewState -> %% Keep for backward compatability reasons
	    {Config, Hook#ct_hook_config{ state = NewState } }
    end.    

call_terminate(#ct_hook_config{ module = Mod, state = State} = Hook, _, _) ->
    catch_apply(Mod,terminate,[State], ok),
    {[],Hook}.

call_cleanup(#ct_hook_config{ module = Mod, state = State} = Hook,
	     Reason, [Function | Args]) ->
    NewState = catch_apply(Mod,Function, Args ++ [Reason, State],
			   State, true),
    {Reason, Hook#ct_hook_config{ state = NewState } }.

call_generic(Hook, Value, Meta) ->
    do_call_generic(Hook, Value, Meta, false).

call_generic_fallback(Hook, Value, Meta) ->
    do_call_generic(Hook, Value, Meta, true).

do_call_generic(#ct_hook_config{ module = Mod, state = State} = Hook,
                Value, [Function | Args], Fallback) ->
    {NewValue, NewState} = catch_apply(Mod, Function, Args ++ [Value, State],
				       {Value,State}, Fallback),
    {NewValue, Hook#ct_hook_config{ state = NewState } }.

%% Generic call function
call(Fun, Config, Meta) ->
    maybe_lock(),
    Hooks = get_hooks(),
    Calls = get_new_hooks(Config, Fun) ++
	[{HookId,Fun} || #ct_hook_config{id = HookId} <- Hooks],
    Res = call(resort(Calls,Hooks,Meta),
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
	{Config, #ct_hook_config{ id = NewId } = NewHook} =
	    call_id(Hook, Config, Meta),
	{NewHooks, NewRest} = 
	    case lists:keyfind(NewId, #ct_hook_config.id, Hooks) of
		false when NextFun =:= undefined ->
		    {Hooks ++ [NewHook],
		     Rest ++ [{NewId, call_init}]};
		ExistingHook when is_tuple(ExistingHook) ->
		    {Hooks, Rest};
		_ ->
		    {Hooks ++ [NewHook],
		     Rest ++ [{NewId, call_init}, {NewId,NextFun}]}
	    end,
	call(resort(NewRest,NewHooks,Meta), Config, Meta, NewHooks)
    catch Error:Reason:Trace ->
	    ct_logs:log("Suite Hook","Failed to start a CTH: ~tp:~tp",
			[Error,{Reason,Trace}]),
	    call([], {fail,"Failed to start CTH"
		      ", see the CT Log for details"}, Meta, Hooks)
    end;
call([{HookId, call_init} | Rest], Config, Meta, Hooks) ->
    call([{HookId, fun call_init/3} | Rest], Config, Meta, Hooks);
call([{HookId, Fun} | Rest], Config, Meta, Hooks) ->
    try
        Hook = lists:keyfind(HookId, #ct_hook_config.id, Hooks),
        {NewConf, NewHook} =  Fun(Hook, Config, Meta),
        NewCalls = get_new_hooks(NewConf, Fun),
        NewHooks = lists:keyreplace(HookId, #ct_hook_config.id, Hooks, NewHook),
        call(resort(NewCalls ++ Rest,NewHooks,Meta), %% Resort if call_init changed prio
	     remove(?config_name, NewConf), Meta,
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

%% Translate scopes, i.e. is_tuplenit_per_group,group1 -> end_per_group,group1 etc
scope([pre_init_per_testcase, SuiteName, TC|_]) ->
    [post_init_per_testcase, SuiteName, TC];
scope([pre_end_per_testcase, SuiteName, TC|_]) ->
    [post_end_per_testcase, SuiteName, TC];
scope([pre_init_per_group, SuiteName, GroupName|_]) ->
    [post_end_per_group, SuiteName, GroupName];
scope([post_init_per_group, SuiteName, GroupName|_]) ->
    [post_end_per_group, SuiteName, GroupName];
scope([pre_init_per_suite, SuiteName|_]) ->
    [post_end_per_suite, SuiteName];
scope([post_init_per_suite, SuiteName|_]) ->
    [post_end_per_suite, SuiteName];
scope(init) ->
    none.

strip_config([post_init_per_testcase, SuiteName, TC|_]) ->
    [post_init_per_testcase, SuiteName, TC];
strip_config([post_end_per_testcase, SuiteName, TC|_]) ->
    [post_end_per_testcase, SuiteName, TC];
strip_config([post_init_per_group, SuiteName, GroupName|_]) ->
    [post_init_per_group, SuiteName, GroupName];
strip_config([post_end_per_group, SuiteName, GroupName|_]) ->
    [post_end_per_group, SuiteName, GroupName];
strip_config([post_init_per_suite, SuiteName|_]) ->
    [post_init_per_suite, SuiteName];
strip_config([post_end_per_suite, SuiteName|_]) ->
    [post_end_per_suite, SuiteName];
strip_config(Other) ->
    Other.


terminate_if_scope_ends(HookId, [on_tc_skip,Suite,{end_per_group,Name}],
			Hooks) ->
    terminate_if_scope_ends(HookId, [post_end_per_group, Suite, Name], Hooks);
terminate_if_scope_ends(HookId, [on_tc_skip,Suite,end_per_suite], Hooks) ->
    terminate_if_scope_ends(HookId, [post_end_per_suite, Suite], Hooks);
terminate_if_scope_ends(HookId, Function0, Hooks) ->
    Function = strip_config(Function0),
    case lists:keyfind(HookId, #ct_hook_config.id, Hooks) of
        #ct_hook_config{ id = HookId, scope = Function} = Hook ->
            terminate([Hook]),
            lists:keydelete(HookId, #ct_hook_config.id, Hooks);
        _ ->
            Hooks
    end.

%% Fetch hook functions
get_new_hooks(Config, Fun) ->
    lists:map(fun(NewHook) when is_atom(NewHook) ->
		      {#ct_hook_config{ module = NewHook }, call_id, Fun};
		 ({NewHook,Opts}) ->
		      {#ct_hook_config{ module = NewHook,
					opts = Opts}, call_id, Fun};
		 ({NewHook,Opts,Prio}) ->
		      {#ct_hook_config{ module = NewHook,
					opts = Opts,
					prio = Prio }, call_id, Fun}
		end, get_new_hooks(Config)).

get_new_hooks(Config) when is_list(Config) ->
    lists:flatmap(fun({?config_name, HookConfigs}) when is_list(HookConfigs) ->
			  HookConfigs;
		     ({?config_name, HookConfig}) when is_atom(HookConfig) ->
			  [HookConfig];
		     (_) ->
			  []
		  end, Config);
get_new_hooks(_Config) ->
    [].

get_builtin_hooks(Opts) ->
    case proplists:get_value(enable_builtin_hooks,Opts) of
	false ->
	    [];
	_Else ->
	    [{HookConf, call_id, undefined} || HookConf <- ?BUILTIN_HOOKS]
    end.

save_suite_data_async(Hooks) ->
    ct_util:save_suite_data_async(?config_name, Hooks).

get_hooks() ->
    lists:keysort(#ct_hook_config.prio,ct_util:read_suite_data(?config_name)).

%% Sort all calls in this order:
%% call_id < call_init < ctfirst < Priority 1 < .. < Priority N < ctlast
%% If Hook Priority is equal, check when it has been installed and
%% sort on that instead.
%% If we are doing a cleanup call i.e. {post,pre}_end_per_*, all priorities
%% are reversed. Probably want to make this sorting algorithm pluginable
%% as some point...
resort(Calls,Hooks,[F|_R]) when F == pre_end_per_testcase;
				F == post_end_per_testcase;
				F == pre_end_per_group;
				F == post_end_per_group;
				F == pre_end_per_suite;
				F == post_end_per_suite ->
    lists:reverse(resort(Calls,Hooks));
resort(Calls,Hooks,_Meta) ->
    resort(Calls,Hooks).
    
resort(Calls, Hooks) ->
    lists:sort(
      fun({_,_,_},_) ->
	      true;
	 (_,{_,_,_}) ->
	      false;
	 ({_,call_init},_) ->
	      true;
	 (_,{_,call_init}) ->
	      false;
	 ({Id1,_},{Id2,_}) ->
	      P1 = (lists:keyfind(Id1, #ct_hook_config.id, Hooks))#ct_hook_config.prio,
	      P2 = (lists:keyfind(Id2, #ct_hook_config.id, Hooks))#ct_hook_config.prio,
	      if
		  P1 == P2 ->
		      %% If priorities are equal, we check the position in the
		      %% hooks list
		      pos(Id1,Hooks) < pos(Id2,Hooks);
		  P1 == ctfirst ->
		      true;
		  P2 == ctfirst ->
		      false;
		  P1 == ctlast ->
		      false;
		  P2 == ctlast ->
		      true;
		  true ->
		      P1 < P2
	      end
      end,Calls).

pos(Id,Hooks) ->
    pos(Id,Hooks,0).
pos(Id,[#ct_hook_config{ id = Id}|_],Num) ->
    Num;
pos(Id,[_|Rest],Num) ->
    pos(Id,Rest,Num+1).


catch_apply(M,F,A, Default) ->
    catch_apply(M,F,A,Default,false).
catch_apply(M,F,A, Default, Fallback) ->
    not erlang:module_loaded(M) andalso (catch M:module_info()),
    case erlang:function_exported(M,F,length(A)) of
        false when Fallback ->
            catch_apply(M,F,tl(A),Default,false);
        false ->
            Default;
        true ->
            catch_apply(M,F,A)
    end.

catch_apply(M,F,A) ->
    try
        erlang:apply(M,F,A)
    catch _:Reason:Trace ->
            ct_logs:log("Suite Hook","Call to CTH failed: ~w:~tp",
                            [error,{Reason,Trace}]),
            throw({error_in_cth_call,
                   lists:flatten(
                     io_lib:format("~w:~tw/~w CTH call failed",
                                   [M,F,length(A)]))})
    end.


%% We need to lock around the state for parallel groups only. This is because
%% we will get several processes reading and writing the state for a single
%% cth at the same time.
maybe_start_locker(Mod,GroupName,Opts) ->
    case lists:member(parallel,Opts) of
	true ->
	    {ok, _Pid} = ct_hooks_lock:start({Mod,GroupName}),
	    ok;
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
