%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
%%% <p>This module is a proxy for calling and handling suite callbacks.</p>

-module(ct_suite_callback).

%% API Exports
-export([init/1]).
-export([init_tc/3]).
-export([end_tc/5]).
-export([terminate/1]).
-export([on_tc_skip/2]).
-export([on_tc_fail/2]).

-type proplist() :: [{atom(),term()}].

-define(config_name, suite_callbacks).

%% -------------------------------------------------------------------------
%% API Functions
%% -------------------------------------------------------------------------

%% @doc Called before any suites are started
-spec init(State :: term()) -> ok |
			       {error, Reason :: term()}.
init(Opts) ->
    call([{CB, call_id, undefined} || CB <- get_new_callbacks(Opts)],
	 ok, init, []).
		      

%% @doc Called after all suites are done.
-spec terminate(Callbacks :: term()) ->
    ok.
terminate(Callbacks) ->
    call([{CBId, fun call_terminate/3} || {CBId,_,_} <- Callbacks],
	 ct_suite_callback_terminate_dummy, terminate, Callbacks),
    ok.

%% @doc Called as each test case is started. This includes all configuration
%% tests.
-spec init_tc(Mod :: atom(), Func :: atom(), Args :: list()) ->
    NewConfig :: proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {fail, Reason :: term()}.
init_tc(ct_framework, _Func, Args) ->
    Args;
init_tc(Mod, init_per_suite, Config) ->
    Info = case catch proplists:get_value(suite_callbacks, Mod:suite()) of
	       List when is_list(List) -> 
		   [{suite_callbacks,List}];
	       _Else ->
		   []
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
    NewConfig :: proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {fail, Reason :: term()} |
    ok.
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

on_tc_skip(How, {_Suite, Case, Reason}) ->
    call(fun call_cleanup/3, {How, Reason}, [on_tc_skip, Case]).

on_tc_fail(_How, {_Suite, Case, Reason}) ->
    call(fun call_cleanup/3, Reason, [on_tc_fail, Case]).

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

call_cleanup({Mod, State}, Reason, [Function | Args]) ->
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
    CBs = get_callbacks(),
    Res = call([{CBId,Fun} || {CBId,_, _} <- CBs] ++
	       get_new_callbacks(Config, Fun),
	       remove(?config_name,Config), Meta, CBs),
    maybe_unlock(),
    Res.

call(Fun, Config, Meta, NoChangeRet) when is_function(Fun) ->
    case call(Fun,Config,Meta) of
	Config -> NoChangeRet;
	NewReturn -> NewReturn
    end;

call([{CB, call_id, NextFun} | Rest], Config, Meta, CBs) ->
    try
	{Config, {NewId, _, _} = NewCB} = call_id(CB, Config, Meta),
	{NewCBs, NewRest} = 
	    case lists:keyfind(NewId, 1, CBs) of
		false when NextFun =:= undefined ->
		    {CBs ++ [NewCB],
		     [{NewId, fun call_init/3} | Rest]};
		ExistingCB when is_tuple(ExistingCB) ->
		    {CBs, Rest};
		_ ->
		    {CBs ++ [NewCB],
		     [{NewId, fun call_init/3},{NewId,NextFun} | Rest]}
	    end,
	call(NewRest, Config, Meta, NewCBs)
    catch Error:Reason ->
	    Trace = erlang:get_stacktrace(),
	    ct_logs:log("Suite Callback","Failed to start a SCB: ~p:~p",
			[Error,{Reason,Trace}]),
	    call([], {fail,"Failed to start SCB"
		      ", see the CT Log for details"}, Meta, CBs)
    end;
call([{CBId, Fun} | Rest], Config, Meta, CBs) ->
    try
        {_,Scope,ModState} = lists:keyfind(CBId, 1, CBs),
        {NewConf, NewCBInfo} =  Fun(ModState, Config, Meta),
        NewCalls = get_new_callbacks(NewConf, Fun),
        NewCBs = lists:keyreplace(CBId, 1, CBs, {CBId, Scope, NewCBInfo}),
        call(NewCalls  ++ Rest, remove(?config_name, NewConf), Meta,
             terminate_if_scope_ends(CBId, Meta, NewCBs))
    catch throw:{error_in_scb_call,Reason} ->
            call(Rest, {fail, Reason}, Meta,
                 terminate_if_scope_ends(CBId, Meta, CBs))
    end;
call([], Config, _Meta, CBs) ->
    save_suite_data_async(CBs),
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

terminate_if_scope_ends(CBId, [Function,Tag|T], CBs) when T =/= [] ->
    terminate_if_scope_ends(CBId,[Function,Tag],CBs);
terminate_if_scope_ends(CBId, Function, CBs) ->
    case lists:keyfind(CBId, 1, CBs) of
        {CBId, Function, _ModState} = CB ->
            terminate([CB]),
            lists:keydelete(CBId, 1, CBs);
        _ ->
            CBs
    end.

%% Fetch callback functions
get_new_callbacks(Config, Fun) ->
    lists:foldl(fun(NewCB, Acc) ->
			[{NewCB, call_id, Fun} | Acc]
		end, [], get_new_callbacks(Config)).

get_new_callbacks(Config) when is_list(Config) ->
    lists:flatmap(fun({?config_name, CallbackConfigs}) ->
			  CallbackConfigs;
		     (_) ->
			  []
		  end, Config);
get_new_callbacks(_Config) ->
    [].

save_suite_data_async(CBs) ->
    ct_util:save_suite_data_async(?config_name, CBs).

get_callbacks() ->
    ct_util:read_suite_data(?config_name).

catch_apply(M,F,A, Default) ->
    try
	apply(M,F,A)
    catch error:Reason ->
	    case erlang:get_stacktrace() of
            %% Return the default if it was the SCB module which did not have the function.
		[{M,F,A}|_] when Reason == undef ->
		    Default;
		Trace ->
		    ct_logs:log("Suite Callback","Call to SCB failed: ~p:~p",
				[error,{Reason,Trace}]),
		    throw({error_in_scb_call,
			   lists:flatten(
			     io_lib:format("~p:~p/~p SCB call failed",
					   [M,F,length(A)]))})
	    end
    end.


%% We need to lock around the state for parallel groups only. This is because
%% we will get several processes reading and writing the state for a single
%% scb at the same time.
maybe_start_locker(Mod,GroupName,Opts) ->
    case lists:member(parallel,Opts) of
	true ->
	    {ok, _Pid} = ct_suite_callback_lock:start({Mod,GroupName});
	false ->
	    ok
    end.

maybe_stop_locker(Mod,GroupName,Opts) ->
    case lists:member(parallel,Opts) of
	true ->
	    stopped = ct_suite_callback_lock:stop({Mod,GroupName});
	false ->
	    ok
    end.


maybe_lock() ->
    locked = ct_suite_callback_lock:request().

maybe_unlock() ->
    unlocked = ct_suite_callback_lock:release().
