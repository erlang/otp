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
-export([terminate/2]).

-type proplist() :: [{atom(),term()}].

-define(config_name, suite_callbacks).

%% API Functions

%% @doc Called before any suites are started
-spec init(State :: term()) -> ok |
			       {error, Reason :: term()}.
init(Opts) ->
    call([{CB, call_init, undefined} || CB <- get_new_callbacks(Opts)],
	 ct_suite_callback_init_dummy, []),
    ok.
		      

%% @doc Called after all suites are done.
-spec terminate(Config :: proplist(),State :: term()) ->
    ok.
terminate(Config, State) ->
    ok.

%% @doc Called as each test case is started. This includes all configuration
%% tests.
-spec init_tc(Mod :: atom(), Func :: atom(), Config :: proplist()) ->
    NewConfig :: proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {error, Reason :: term()}.
init_tc(Mod, init_per_suite, Config) ->
    call(fun call_pre_init_suite/2, Config);
init_tc(_, _, Config) ->
    Config.

%% @doc Called as each test case is completed. This includes all configuration
%% tests.
-spec end_tc(Mod :: atom(),
	     Func :: atom(),
	     CallConfig :: proplist(),
	     Result :: term()) ->
    NewConfig :: proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {error, Reason :: term()} |
    ok.
end_tc(Mod, init_per_suite, _, Result) ->
    
    NewConfig = call(fun call_post_init_suite/2, Result),
    
    NewConfig;
end_tc(_, _, _, Result) ->
    Result.


%% Iternal Functions
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

call_init(Mod, Config) when is_atom(Mod) ->
    call_init({Mod, undefined}, Config);
call_init({Mod, State}, Config) ->
    {Id, NewState} = Mod:init(State),
    {Config, {Id, {Mod, NewState}}}.

call_pre_init_suite({Mod, State}, Config) ->
    {NewConf, NewState} = Mod:pre_init_suite(Config, State),
    {NewConf, {Mod, NewState}}.
call_post_init_suite({Mod, State}, Config) ->
    {NewConf, NewState} = Mod:post_init_suite(Config, State),
    {NewConf, {Mod, NewState}}.

%% Generic call function
call(Fun, Config) ->
    CBs = get_callbacks(),
    call([{CBId,Fun} || {CBId, _} <- CBs] ++ get_new_callbacks(Config, Fun),
	 remove(?config_name,Config), CBs).
	 
call([{CB, call_init, NextFun} | Rest], Config, CBs) ->
    {Config, {NewId, _} = NewCB} = call_init(CB, Config),
    {NewCBs, NewRest} = case proplists:get_value(NewId, CBs, NextFun) of
			    undefined -> {CBs ++ [NewCB],Rest};
			    {NewId, _, _} -> {CBs, Rest};
			    Fun -> {CBs ++ [NewCB],[{NewId, NextFun} | Rest]}
			end,
    call(NewRest, Config, NewCBs);
call([{CBId, Fun} | Rest], Config, CBs) ->
    {NewConf, NewCBInfo} =  Fun(proplists:get_value(CBId, CBs), Config),
    NewCalls = get_new_callbacks(NewConf, Fun),
    call(NewCalls  ++ Rest, remove(?config_name, NewConf),
	 lists:keyreplace(CBId, 1, CBs, {CBId, NewCBInfo}));
call([], Config, CBs) ->
    ct_util:save_suite_data_async(?config_name, CBs),
    Config.

remove(Key,List) when is_list(List) ->
    [Conf || Conf <- List,
	     element(1, Conf) =/= Key];
remove(_, Else) ->
    Else.
