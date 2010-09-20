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
    call(get_new_callbacks(Opts), fun call_init/2, ok).
		      

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
    NewConfig = call(get_new_callbacks(Config) ++ get_callbacks(),
		     fun call_init/2, remove(?config_name,Config)),
    
    Data = ct_util:read_suite_data(?config_name),
    [{suitedata, Data} | NewConfig];
init_tc(Mod, Func, Config) ->
    Config.

%% @doc Called as each test case is completed. This includes all configuration
%% tests.
-spec end_tc(Mod :: atom(),
	     Func :: atom(),
	     Config :: proplist(),
	     Result :: term()) ->
    NewConfig :: proplist() |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {error, Reason :: term()} |
    ok.
end_tc(Mod, init_per_suite, _, Return) ->
    NewConfig = call(get_new_callbacks(Return) ++ get_callbacks(),
		     fun call_init/2, remove(suitedata, remove(?config_name,Return))),
    
    Data = ct_util:read_suite_data(?config_name),
    [{suitedata, Data} | NewConfig];
end_tc(Mod, Func, Config, Result) ->
    Result.


%% Iternal Functions
get_new_callbacks(Config) ->
    lists:flatmap(fun({?config_name, CallbackConfigs}) ->
			  CallbackConfigs;
		     (_) ->
			  []
		  end, Config).

get_callbacks() ->
    ct_util:read_suite_data(?config_name).

call_init(Mod, Config) when is_atom(Mod) ->
    call_init({Mod, undefined}, Config);
call_init({Mod, State}, Config) ->
    {{Mod, running, Mod:init(State)}, Config};
call_init({Mod, running, State}, Config) ->
    {{Mod, running, State}, Config}.

%% Generic call function
call(Fun, Config) ->
    call(get_callbacks(), Fun, Config).

call(CBs, Fun, Config) ->
    call(CBs, Fun, Config, []).
    
call([CB | Rest], Fun, Config, NewCBs) ->
    {NewCB, NewConf} = Fun(CB,Config),
    call(Rest, Fun, NewConf, [NewCB | NewCBs]);
call([], _Fun, Config, NewCBs) ->
    ct_util:save_suite_data_async(?config_name, NewCBs),
    Config.


remove(Key,List) ->
    [Conf || Conf <- List,
	     element(1,Conf) =/= Key].
