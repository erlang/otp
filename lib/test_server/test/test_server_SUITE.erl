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
%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(test_server_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("test_server_test_lib.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%% @spec suite() -> Info
suite() ->
    [{ct_hooks,[ts_install_cth,test_server_test_lib]}].


%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
init_per_suite(Config) ->
    [{path_dirs,[proplists:get_value(data_dir,Config)]} | Config].

%% @spec end_per_suite(Config) -> _
end_per_suite(_Config) ->
    io:format("TEST_SERVER_FRAMEWORK: ~p",[os:getenv("TEST_SERVER_FRAMEWORK")]),
    ok.

%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
init_per_group(_GroupName, Config) ->
    Config.

%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
end_per_group(_GroupName, _Config) ->
    ok.

%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
init_per_testcase(_TestCase, Config) ->
    Config.

%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @spec: groups() -> [Group]
groups() ->
    [].

%% @spec all() -> GroupsAndTestCases | {skip,Reason}
all() ->
    [test_server_SUITE, test_server_parallel01_SUITE,
     test_server_conf02_SUITE, test_server_conf01_SUITE,
     test_server_skip_SUITE, test_server_shuffle01_SUITE].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
test_server_SUITE(Config) ->
%    rpc:call(Node,dbg, tracer,[]),
%    rpc:call(Node,dbg, p,[all,c]),
%    rpc:call(Node,dbg, tpl,[test_server_ctrl,x]),
    run_test_server_tests("test_server_SUITE", 39, 1, 31, 
			  20, 9, 1, 11, 2, 26, Config).

test_server_parallel01_SUITE(Config) ->
    run_test_server_tests("test_server_parallel01_SUITE", 37, 0, 19, 
			  19, 0, 0, 0, 0, 37, Config).

test_server_shuffle01_SUITE(Config) ->
    run_test_server_tests("test_server_shuffle01_SUITE", 130, 0, 0, 
			  76, 0, 0, 0, 0, 130, Config).

test_server_skip_SUITE(Config) ->
    run_test_server_tests("test_server_skip_SUITE", 3, 0, 1, 
			  0, 0, 1, 3, 0, 0, Config).

test_server_conf01_SUITE(Config) ->
    run_test_server_tests("test_server_conf01_SUITE", 24, 0, 12, 
			  12, 0, 0, 0, 0, 24, Config).

test_server_conf02_SUITE(Config) ->
    run_test_server_tests("test_server_conf02_SUITE", 26, 0, 12, 
			  12, 0, 0, 0, 0, 26, Config).


run_test_server_tests(SuiteName, NCases, NFail, NExpected, NSucc, 
		      NUsrSkip, NAutoSkip, 
		      NActualSkip, NActualFail, NActualSucc, Config) ->

    ct:log("See test case log files under:~n~p~n",
	   [filename:join([proplists:get_value(priv_dir, Config),
			   SuiteName++".logs"])]),

    Node = proplists:get_value(node, Config),
    {ok,_Pid} = rpc:call(Node,test_server_ctrl, start, []),
    rpc:call(Node,
	     test_server_ctrl,add_dir_with_skip,
	     [SuiteName, 
	      [proplists:get_value(data_dir,Config)],SuiteName,
	      [{test_server_SUITE,skip_case7,"SKIPPED!"}]]),

    until(fun() ->
		  rpc:call(Node,test_server_ctrl,jobs,[]) =:= []
	  end),
    
    rpc:call(Node,test_server_ctrl, stop, []),

    {ok,#suite{ n_cases = NCases,
		n_cases_failed = NFail,
		n_cases_expected = NExpected, 
		n_cases_succ = NSucc,
		n_cases_user_skip = NUsrSkip,
		n_cases_auto_skip = NAutoSkip,
		cases = Cases }} = Data =
	test_server_test_lib:parse_suite(
	  hd(filelib:wildcard(
	       filename:join([proplists:get_value(priv_dir, Config), 
			      SuiteName++".logs","run*","suite.log"])))),
    {NActualSkip,NActualFail,NActualSucc} = 
	lists:foldl(fun(#tc{ result = skip },{S,F,Su}) ->
			     {S+1,F,Su};
			 (#tc{ result = ok },{S,F,Su}) ->
			     {S,F,Su+1};
			(#tc{ result = failed },{S,F,Su}) ->
			     {S,F+1,Su}
			  end,{0,0,0},Cases),
    Data.

until(Fun) ->
    case Fun() of
	true ->
	    ok;
	false ->
	    timer:sleep(100),
	    until(Fun)
    end.
	  
