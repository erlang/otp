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
-export([end_tc/4]).
-export([terminate/1]).

-type proplist() :: [{atom(),term()}].

-define(config_name, suite_callbacks).

%% -------------------------------------------------------------------------
%% API Functions
%% -------------------------------------------------------------------------

%% @doc Called before any suites are started
-spec init(State :: term()) -> ok |
			       {error, Reason :: term()}.
init(Opts) ->
    call([{CB, call_init, undefined} || CB <- get_new_callbacks(Opts)],
	 ct_suite_callback_init_dummy, undefined, []),
    ok.
		      

%% @doc Called after all suites are done.
-spec terminate(Callbacks :: term()) ->
    ok.
terminate(Callbacks) ->
    io:format("Callbacks: ~p",[Callbacks]),
    call([{CBId, fun call_terminate/3} || {CBId,_} <- Callbacks],
	 ct_suite_callback_init_dummy, undefined, Callbacks),
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
    call(fun call_generic/3, Config, {pre_init_per_suite, Mod});
init_tc(Mod, end_per_suite, Config) ->
    call(fun call_generic/3, Config, {pre_end_per_suite, Mod});
init_tc(_Mod, {init_per_group, GroupName, _}, Config) ->
    call(fun call_generic/3, Config, {pre_init_per_group, GroupName});
init_tc(_Mod, {end_per_group, GroupName, _}, Config) ->
    call(fun call_generic/3, Config, {pre_end_per_group, GroupName});
init_tc(_Mod, TC, Config) ->
    call(fun call_generic/3, Config, {pre_init_per_testcase, TC}).

%% @doc Called as each test case is completed. This includes all configuration
%% tests.
-spec end_tc(Mod :: atom(),
	     Func :: atom(),
	     Args :: list(),
	     Result :: term()) ->
    NewConfig :: proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {fail, Reason :: term()} |
    ok.
end_tc(ct_framework, _Func, _Args, Result) ->
    Result;
end_tc(Mod, init_per_suite, _Config, Result) ->
    call(fun call_generic/3, Result, {post_init_per_suite, Mod});
end_tc(Mod, end_per_suite, _Config, Result) ->
    call(fun call_generic/3, Result, {post_end_per_suite, Mod});
end_tc(_Mod, {init_per_group, GroupName, _}, _Config, Result) ->
    call(fun call_generic/3, Result, {post_init_per_group, GroupName});
end_tc(_Mod, {end_per_group, GroupName, _}, _Config, Result) ->
    call(fun call_generic/3, Result, {post_end_per_group, GroupName});
end_tc(_Mod, TC, _Config, Result) ->
    call(fun call_generic/3, Result, {post_end_per_testcase, TC}).

%% -------------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------------
call_init(Mod, Config, Meta) when is_atom(Mod) ->
    call_init({Mod, undefined}, Config, Meta);
call_init({Mod, State}, Config, _) ->
    {Id, NewState} = Mod:init(State),
    {Config, {Id, {Mod, NewState}}}.

call_terminate({Mod, State}, _, _) ->
    Mod:terminate(State),
    {[],{Mod,State}}.

call_generic({Mod, State}, Config, {Function, undefined}) ->
    {NewConf, NewState} = Mod:Function(Config, State),
    {NewConf, {Mod, NewState}};
call_generic({Mod, State}, Config, {Function, Tag}) ->
    {NewConf, NewState} = Mod:Function(Tag, Config, State),
    {NewConf, {Mod, NewState}}.

%% Generic call function
call(Fun, Config, Meta) ->
    CBs = get_callbacks(),
    call([{CBId,Fun} || {CBId, _} <- CBs] ++ get_new_callbacks(Config, Fun),
	     remove(?config_name,Config), Meta, CBs).

call([{CB, call_init, NextFun} | Rest], Config, Meta, CBs) ->
    {Config, {NewId, _} = NewCB} = call_init(CB, Config, Meta),
    {NewCBs, NewRest} = case proplists:get_value(NewId, CBs, NextFun) of
			    undefined -> {CBs ++ [NewCB],Rest};
			    ExistingCB when is_tuple(ExistingCB) -> {CBs, Rest};
			    _ -> {CBs ++ [NewCB],[{NewId, NextFun} | Rest]}
			end,
    call(NewRest, Config, Meta, NewCBs);
call([{CBId, Fun} | Rest], Config, Meta, CBs) ->
    {NewConf, NewCBInfo} =  Fun(proplists:get_value(CBId, CBs), Config, Meta),
    NewCalls = get_new_callbacks(NewConf, Fun),
    call(NewCalls  ++ Rest, remove(?config_name, NewConf), Meta,
	 lists:keyreplace(CBId, 1, CBs, {CBId, NewCBInfo}));
call([], Config, _Meta, CBs) ->
    ct_util:save_suite_data_async(?config_name, CBs),
    Config.

remove(Key,List) when is_list(List) ->
    [Conf || Conf <- List, is_tuple(Conf) =:= false
		 orelse element(1, Conf) =/= Key];
remove(_, Else) ->
    Else.

%% Fetch callback functions
get_new_callbacks(Config, Fun) ->
    lists:foldl(fun(NewCB, Acc) ->
			[{NewCB, call_init, Fun} | Acc]
		end, [], get_new_callbacks(Config)).

get_new_callbacks(Config) when is_list(Config) ->
    lists:flatmap(fun({?config_name, CallbackConfigs}) ->
			  CallbackConfigs;
		     (_) ->
			  []
		  end, Config);
get_new_callbacks(_Config) ->
    [].

get_callbacks() ->
    ct_util:read_suite_data(?config_name).
