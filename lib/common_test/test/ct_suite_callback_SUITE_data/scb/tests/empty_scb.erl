%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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

%%% @doc Common Test Example Suite Callback module.
%%%
%%% <p>This module gives an example of a common test SCB (Suite CallBack).
%%% There are many ways to add a SCB to a test run, you can do it either in
%%% the command line using -suite_callback, in a test spec using
%%% {suite_callback,M} or in the suite it self by returning suite_callback
%%% from either suite/0, init_per_suite/1, init_per_group/2 and
%%% init_per_testcase/2. The scope of the SCB is determined by where is it
%%% started. If it is started in the command line or test spec then it will
%%% be stopped at the end of all tests. If it is started in init_per_suite,
%%% it will be stopped after end_per_suite and so on. See terminate
%%% documentation for a table describing the scoping machanics. 
%%%
%%% All of callbacks except init/1 in a SCB are optional.</p>

-module(empty_scb).

%% Suite Callbacks
-export([init/1]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/3]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/3]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/3]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/3]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/3]).

-export([on_tc_fail/3]).

-export([terminate/1]).

-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

-type proplist() :: list({atom(),term()}).
-type config() :: proplist().
-type reason() :: term().
-type skip_or_fail() :: {skip, reason()} |
                        {auto_skip, reason()} |
                        {fail, reason()}.

-record(state, { id = ?MODULE :: term()}).

%% @doc Always called before any other callback function. Use this to initiate
%% any common state. It should return an ID for this SCB and a state. The ID
%% is used to uniquly identify an SCB instance, if two SCB's return the same
%% ID the seconds SCB is ignored. This function should NOT have any side
%% effects as it might be called multiple times by common test.
-spec init(Opts :: proplist()) ->
    {Id :: term(), State :: #state{}}.
init(Opts) ->
    gen_event:notify(?CT_EVMGR_REF, #event{ name = scb, node = node(),
					    data = {?MODULE, init, [Opts]}}),
    {now(), Opts}.

%% @doc Called before init_per_suite is called. Note that this callback is
%% only called if the SCB is added before init_per_suite is run (eg. in a test
%% specification, suite/0 function etc).
%% You can change the config in the this function.
-spec pre_init_per_suite(Suite :: atom(),
		     Config :: config(),
		     State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_suite(Suite,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, pre_init_per_suite,
				     [Suite,State]}}),
    {Config, State}.

%% @doc Called after init_per_suite.
%% you can change the config in this function.
-spec post_init_per_suite(Suite :: atom(),
		      Config :: config(),
		      State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
post_init_per_suite(Suite,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, post_init_per_suite,
				     [Suite,State]}}),
    {Config, State}.

%% @doc Called before end_per_suite. Note that the config cannot be
%% changed here, only the status of the suite.
-spec pre_end_per_suite(Suite :: atom(),
		    Config :: config(),
		    State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
pre_end_per_suite(Suite,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, pre_end_per_suite,
				     [Suite,State]}}),
    {Config, State}.

%% @doc Called after end_per_suite. Note that the config cannot be
%% changed here, only the status of the suite.
-spec post_end_per_suite(Suite :: atom(),
		     Config :: config(),
		     State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_suite(Suite,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, post_end_per_suite,
				     [Suite,State]}}),
    {Config, State}.

%% @doc Called before each init_per_group.
%% You can change the config in this function.
-spec pre_init_per_group(Group :: atom(),
		     Config :: config(),
		     State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_group(Group,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, pre_init_per_group,
				     [Group,State]}}),
    {Config, State}.

%% @doc Called after each init_per_group.
%% You can change the config in this function.
-spec post_init_per_group(Group :: atom(),
		      Config :: config(),
		      State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
post_init_per_group(Group,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, post_init_per_group,
				     [Group,State]}}),
    {Config, State}.

%% @doc Called after each end_per_group. Note that the config cannot be
%% changed here, only the status of the group.
-spec pre_end_per_group(Group :: atom(),
		    Config :: config(),
		    State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
pre_end_per_group(Group,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, pre_end_per_group,
				     [Group,State]}}),
    {Config, State}.

%% @doc Called after each end_per_group. Note that the config cannot be
%% changed here, only the status of the group.
-spec post_end_per_group(Group :: atom(),
		     Config :: config(),
		     State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_group(Group,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, post_end_per_group,
				     [Group,State]}}),
    {Config, State}.

%% @doc Called before each test case.
%% You can change the config in this function.
-spec pre_init_per_testcase(TC :: atom(),
		  Config :: config(),
		  State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_testcase(TC,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, pre_init_per_testcase,
				     [TC,State]}}),
    {Config, State}.

%% @doc Called after each test case. Note that the config cannot be
%% changed here, only the status of the test case.
-spec post_end_per_testcase(TC :: atom(),
		  Config :: config(),
		  State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_testcase(TC,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, post_end_per_testcase,
				     [TC,State]}}),
    {Config, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_tc if the suite, group or test case failed.
%% This function should be used for extra cleanup which might be needed.
%% It is not possible to modify the config or the status of the test run.
-spec on_tc_fail(TC :: init_per_suite | end_per_suite |
		 init_per_group | end_per_group | atom(),
		 Config :: config(), State :: #state{}) ->
    ok.
on_tc_fail(TC, Config, State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, on_tc_fail,
				     [TC,State]}}),
    ok.

%% @doc Called when the scope of the SCB is done, this depends on
%% when the SCB was specified. This translation table describes when this
%% function is called.
%%
%%  | Started in          |     terminate called    |
%%  |---------------------|-------------------------|
%%  | command_line        | after all tests are run |
%%  | test spec           | after all tests are run |
%%  | suite/0             | after SUITE is done     |
%%  | init_per_suite/1    | after SUITE is done     |
%%  | init_per_group/2    | after group is done     |
%%  | init_per_testcase/2 | after test case is done |
%%  |-----------------------------------------------|
%%
-spec terminate(State :: #state{}) ->
    term().
terminate(State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = scb, node = node(),
			     data = {?MODULE, terminate, [State]}}),
    ok.
