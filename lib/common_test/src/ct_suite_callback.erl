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
    add_new_callbacks(Opts),
    ok.

%% @doc Called after all suites are done.
-spec terminate(Config :: proplist(),State :: term()) ->
    ok.
terminate(Config, State) ->
    ok.

%% @doc Called as each test case is started. This includes all configuration
%% tests.
-spec init_tc(Mod :: atom(), Func :: atom(), Config :: proplist()) ->
    {ok, NewConfig :: proplist()} |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {error, Reason :: term()}.
init_tc(Mod, init_per_suite, Config) ->
    add_new_callbacks(Config),
    {ok, Config};
init_tc(Mod, Func, Config) ->
    {ok, Config}.

%% @doc Called as each test case is completed. This includes all configuration
%% tests.
-spec end_tc(Mod :: atom(),
	     Func :: atom(),
	     Config :: proplist(),
	     Result :: term()) ->
    {ok, NewConfig :: proplist()} |
    {skip, Reason :: term()} |
    {auto_skip, Reason :: term()} |
    {error, Reason :: term()} |
    ok.
end_tc(Mod, Func, Config, Result) ->
    {ok, Config}.


%% Iternal Functions
add_new_callbacks(Config) ->
    NewCBConfs = lists:flatmap(fun({?config_name, CallbackConfigs}) ->
				       CallbackConfigs;
				  (_) ->
				       []
			       end, Config),
    CBStates = lists:map(fun call_init/1,NewCBConfs),
    ct_util:save_suite_data_async(?config_name, CBStates).

call_init({Mod, Config}) ->
    {Mod, Mod:init(Config)}.
	
	    
