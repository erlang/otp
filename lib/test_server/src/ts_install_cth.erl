%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

	%% Make the stuff in all_SUITE_data if it exists
	AllDir = filename:join(DataDir,"../all_SUITE_data"),
	case filelib:is_dir(AllDir) of
	    true ->
		make_non_erlang(AllDir,Variables);
	    false ->
		ok
	end,
	
	make_non_erlang(DataDir, Variables),

	{add_node_name(Config, State), State}
    catch Error:Reason ->
	    Stack = erlang:get_stacktrace(),
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

%% @doc Called after each test case. 
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
%% Configure and run all the Makefiles in the data dirs of the suite 
%% in question
make_non_erlang(DataDir, Variables) ->
    {ok,CurrWD} = file:get_cwd(),
    try
	file:set_cwd(DataDir),
	MakeCommand = proplists:get_value(make_command,Variables),
	
	FirstMakefile = filename:join(DataDir,"Makefile.first"),
	case filelib:is_regular(FirstMakefile) of
	    true ->
		ct:log("Making ~p",[FirstMakefile]),
		ok = ts_make:make(
		       MakeCommand, DataDir, filename:basename(FirstMakefile));
	    false ->
		ok
	end,
	
	MakefileSrc = filename:join(DataDir,"Makefile.src"),
	MakefileDest = filename:join(DataDir,"Makefile"),
	case filelib:is_regular(MakefileSrc) of
	    true ->
		ok = ts_lib:subst_file(MakefileSrc,MakefileDest,Variables),
		ct:log("Making ~p",[MakefileDest]),
		ok = ts_make:make([{makefile,"Makefile"},{data_dir,DataDir} 
				   | Variables]);
	    false ->
		ok
	end
    after
	file:set_cwd(CurrWD),
	timer:sleep(100)
    end.

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
    Name=list_to_atom(temp_nodename("nod", []) ++ "@" ++ Host),
    generate_nodenames2(N-1, Hosts, [Name|Acc]).

temp_nodename([], Acc) ->
    lists:flatten(Acc);
temp_nodename([Chr|Base], Acc) ->
    {A,B,C} = erlang:now(),
    New = [Chr | integer_to_list(Chr bxor A bxor B+A bxor C+B)],
    temp_nodename(Base, [New|Acc]).
