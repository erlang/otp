%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(t11_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{require,file},
     {require,tcname},
     {timetrap,{seconds,1}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->

    TCName = ct:get_config(tcname),
    CfgFiles = ct:get_config(file,undefined,[all]),

    %% verify that expected config file can be read
    case {TCName,CfgFiles} of
	{start_separate,[cfg11]} -> ok;
	{start_join,[cfg11,cfg21]} -> ok;
	{incl_separate1,[cfg11]} -> ok;
	{incl_separate2,[cfg11]} -> ok;
	{incl_join1,[cfg21,cfg11]} -> ok;
	{incl_join1,[cfg12,cfg11,cfg21]} -> ok;
	{incl_join2,[cfg21,cfg11,cfg12]} -> ok;
	{incl_both1,[cfg11]} -> ok;
	{incl_both2,[cfg11,cfg12,cfg21]} -> ok;
	{incl_both2,[cfg11]} -> ok;
	_ -> ok

    end,

    %% test the get_testspec_terms functionality
    if CfgFiles /= undefined ->
	    TSTerms = case ct:get_testspec_terms() of
			  undefined -> exit('testspec should not be undefined');
			  Result -> Result
		      end,
	    true = lists:keymember(config, 1, TSTerms),
	    {config,TSCfgFiles} = ct:get_testspec_terms(config),
	    [{config,TSCfgFiles},{tests,Tests}] = 
		ct:get_testspec_terms([config,tests]),
	    CfgNames = [list_to_atom(filename:basename(TSCfgFile)) ||
			   {Node,TSCfgFile} <- TSCfgFiles, Node == node()],
	    true = (length(CfgNames) == length(CfgFiles)),
	    [true = lists:member(CfgName,CfgFiles) || CfgName <- CfgNames],
	    true = lists:any(fun({{_Node,_Dir},Suites}) ->
				     lists:keymember(?MODULE, 1, Suites)
			     end, Tests);
       true ->
	    ok
    end,

    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(autoskip_tc, Config) ->
    exit(kaboom),
    Config;

init_per_testcase(userskip_tc, Config) ->
    {skip,"user skipped"};

init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [ok_tc, exit_tc, to_tc, autoskip_tc, userskip_tc].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
ok_tc(_) ->
    ok.

exit_tc(_) ->
    exit(kaboom),
    ok.

to_tc(_) ->
    ct:timetrap(1),
    ct:sleep(100),
    ok.

autoskip_tc(_) ->
    ok.

userskip_tc(_) ->
    ok.
    



