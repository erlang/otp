%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

%%% @doc TS Installed SCB
%%%
%%% This module does what the make parts of the ts:run/x command did,
%%% but not the Makefile.first parts! So they have to be done by ts or
%%% manually!!

-module(ts_install_cth).

%% Suite Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_init_per_testcase/4]).
-export([pre_end_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).

-include_lib("kernel/include/file.hrl").

-type config() :: proplists:proplist().
-type reason() :: term().
-type skip_or_fail() :: {skip, reason()} |
                        {auto_skip, reason()} |
                        {fail, reason()}.

-record(state, { ts_conf_dir, target_system, install_opts, nodenames, nodes }).

%% @doc The id of this SCB
-spec id(Opts :: term()) ->
    Id :: term().
id(_Opts) ->
    ?MODULE.

%% @doc Always called before any other callback function.
-spec init(Id :: term(), Opts :: proplists:proplist()) ->
    {ok, State :: #state{}}.
init(_Id, Opts) ->
    Nodenames = proplists:get_value(nodenames, Opts, 0),
    Nodes = proplists:get_value(nodes, Opts, 0),
    TSConfDir = proplists:get_value(ts_conf_dir, Opts),
    TargetSystem = proplists:get_value(target_system, Opts, install_local),
    InstallOpts = proplists:get_value(install_opts, Opts, []),
    {ok, #state{ nodenames = Nodenames,
		 nodes = Nodes,
		 ts_conf_dir = TSConfDir,
		 target_system = TargetSystem, 
		 install_opts = InstallOpts } }.

%% @doc Called before init_per_suite is called.
-spec pre_init_per_suite(Suite :: atom(),
			 Config :: config(),
			 State :: #state{}) ->
	{config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_suite(Suite,Config,#state{ ts_conf_dir = undefined} = State) ->
    DataDir = proplists:get_value(data_dir, Config),
    ParentDir = filename:join(
		  lists:reverse(
		    tl(lists:reverse(filename:split(DataDir))))),
    TSConfDir = filename:join([ParentDir, "..","test_server"]),
    pre_init_per_suite(Suite, Config, State#state{ ts_conf_dir = TSConfDir });
pre_init_per_suite(_Suite,Config,State) ->
    DataDir = proplists:get_value(data_dir, Config),
    try
	{ok,Variables} = 
	    file:consult(filename:join(State#state.ts_conf_dir,"variables")),
	case proplists:get_value(cross,Variables) of
	    "yes" ->
		ct:log("Not making data dir as tests have been cross compiled");
	    _ ->
		ts_lib:make_non_erlang(DataDir, Variables)
	end,

	{add_node_name(Config, State), State}
    catch error:{badmatch,{error,enoent}} ->
	{add_node_name(Config, State), State};
	  Error:Reason:Stack ->
	    ct:pal("~p failed! ~p:{~p,~p}",[?MODULE,Error,Reason,Stack]),
	    {{fail,{?MODULE,{Error,Reason, Stack}}},State}
    end.

%% @doc Called after init_per_suite.
-spec post_init_per_suite(Suite :: atom(),
			  Config :: config(),
			  Return :: config() | skip_or_fail(),
			  State :: #state{}) ->
	{config() | skip_or_fail(), NewState :: #state{}}.
post_init_per_suite(_Suite,_Config,Return,State) ->
    test_server_ctrl:kill_slavenodes(),
    {Return, State}.

%% @doc Called before end_per_suite. 
-spec pre_end_per_suite(Suite :: atom(),
			Config :: config() | skip_or_fail(),
			State :: #state{}) ->
	{ok | skip_or_fail(), NewState :: #state{}}.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite. 
-spec post_end_per_suite(Suite :: atom(),
			 Config :: config(),
			 Return :: term(),
			 State :: #state{}) ->
	{ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
-spec pre_init_per_group(Group :: atom(),
			 Config :: config(),
			 State :: #state{}) ->
	{config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_group(_Group,Config,State) ->
    {add_node_name(Config, State), State}.

%% @doc Called after each init_per_group.
-spec post_init_per_group(Group :: atom(),
			  Config :: config(),
			  Return :: config() | skip_or_fail(),
			  State :: #state{}) ->
	{config() | skip_or_fail(), NewState :: #state{}}.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group. 
-spec pre_end_per_group(Group :: atom(),
			Config :: config() | skip_or_fail(),
			State :: #state{}) ->
	{ok | skip_or_fail(), NewState :: #state{}}.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group. 
-spec post_end_per_group(Group :: atom(),
			 Config :: config(),
			 Return :: term(),
			 State :: #state{}) ->
	{ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each test case.
-spec pre_init_per_testcase(TC :: atom(),
			    Config :: config(),
			    State :: #state{}) ->
	{config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_testcase(_TC,Config,State) ->
    {add_node_name(Config, State), State}.

-spec post_init_per_testcase(TC :: atom(),
			    Config :: config(),
			    Return :: term(),
			    State :: #state{}) ->
	{ok | skip_or_fail(), NewState :: #state{}}.
post_init_per_testcase(_TC,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each test case. 
-spec pre_end_per_testcase(TC :: atom(),
			   Config :: config(),
			   State :: #state{}) ->
	{config() | skip_or_fail(), NewState :: #state{}}.
pre_end_per_testcase(_TC,Config,State) ->
    {Config, State}.

-spec post_end_per_testcase(TC :: atom(),
			    Config :: config(),
			    Return :: term(),
			    State :: #state{}) ->
	{ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_testcase(_TC,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after a test case failed.
-spec on_tc_fail(TC :: init_per_suite | end_per_suite |
		       init_per_group | end_per_group | atom(),
		 Reason :: term(), State :: #state{}) ->
	NewState :: #state{}.
on_tc_fail(_TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped. 
-spec on_tc_skip(TC :: end_per_suite | init_per_group | end_per_group | atom(),
		 {tc_auto_skip, {failed, {Mod :: atom(), Function :: atom(), 
					  Reason :: term()}}} |
		 {tc_user_skip, {skipped, Reason :: term()}},
		 State :: #state{}) ->
	NewState :: #state{}.
on_tc_skip(_TC, _Reason, State) ->
    State.

%% @doc Called when the scope of the SCB is done.
-spec terminate(State :: #state{}) ->
	term().
terminate(_State) ->
    ok.

%%% ============================================================================
%%% Local functions
%%% ============================================================================

%% Add a nodename to config if it does not exist
add_node_name(Config, State) ->
    case proplists:get_value(nodenames, Config) of
	undefined ->
	    lists:keystore(
	       nodenames, 1, Config, 
	       {nodenames,generate_nodenames(State#state.nodenames)});
	_Else ->
	    Config
    end.


%% Copied from test_server_ctrl.erl
generate_nodenames(Num) ->
    {ok,Name} = inet:gethostname(),
    generate_nodenames2(Num, [Name], []).

generate_nodenames2(0, _Hosts, Acc) ->
    Acc;
generate_nodenames2(N, Hosts, Acc) ->
    Host=lists:nth((N rem (length(Hosts)))+1, Hosts),
    Name=list_to_atom(temp_nodename("nod",N) ++ "@" ++ Host),
    generate_nodenames2(N-1, Hosts, [Name|Acc]).

%% We cannot use erlang:unique_integer([positive])
%% here since this code in run on older test releases as well.
temp_nodename(Base,I) ->
    {A,B,C} = os:timestamp(),
    Nstr = integer_to_list(I),
    Astr = integer_to_list(A),
    Bstr = integer_to_list(B),
    Cstr = integer_to_list(C),
    Base++Nstr++Astr++Bstr++Cstr.
