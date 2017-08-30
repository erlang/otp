%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: 
%%%
%%% Description:
%%% 
%%%
%%% The suites used for the test are located in the data directory.
%%%
%%% The group(s) and case(s) are specified according to this:
%%%
%%% Tests = ct_groups:find_groups(Mod, GroupPaths, TestCases, GroupDef)
%%%
%%% GroupPaths = GroupPath | [GroupPath]
%%% GroupPath = atom() | [atom()]
%%%
%%% CT will find all paths that include GroupPath. GroupPath can be a
%%% single group, or a list of groups along the path to TestCases.
%%% If GroupPath is the latter, the last group in the list must be
%%% the "terminating" group in the path, or it will be impossible to
%%% execute test cases in higher level groups *only*, as in this case:
%%% groups() -> [{g1,[],[tc1,{g2,[],[tc2]}]}].
%%% Compare: find_groups(x, g1, all, groups()), and
%%%          find_groups(x, [[g1]], all, groups())
%%%
%%% Some examples:
%%%
%%% GroupPaths = g1, means find all paths with g1 included
%%% GroupPaths = [g1], -''-
%%% GroupPaths = [g1,g2], search twice - once for g1 and once for g2
%%% GroupPaths = [[g1,g2]], find cases under group g1 and sub group g2
%%% GroupPaths = [[g1,g2],[g1,g3]], find cases for g1-g2 AND g1-g3
%%%
%%% TestCases = all | atom() | [atom()]
%%%
%%%-------------------------------------------------------------------

-module(ct_groups_search_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/src/ct_util.hrl").


-define(eh, ct_test_support_eh).

-define(M1, groups_search_dummy_1_SUITE).
-define(M2, groups_search_dummy_2_SUITE).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    code:add_patha(DataDir),
    M1Erl = filename:join(DataDir, atom_to_list(?M1)++".erl"),
    M2Erl = filename:join(DataDir, atom_to_list(?M2)++".erl"),
    {ok,?M1} = compile:file(M1Erl, [{outdir,DataDir}]),
    {ok,?M2} = compile:file(M2Erl, [{outdir,DataDir}]),
    {module,?M1} = code:load_file(?M1),
    {module,?M2} = code:load_file(?M2),

    Config1 = ct_test_support:init_per_suite(Config),
    Config1.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

groups() -> 
    [
     {find_groups,[],[all_groups,
		      testcases_in_all_groups,
		      all_in_top_group1,
		      all_in_top_group2,
		      all_in_sub_group1,
		      all_in_sub_group2,
		      testcase_in_top_group1,
		      testcase_in_top_group2,
		      testcase_in_sub_group1,
		      testcase_in_sub_group2,
		      testcase_in_top_groups1,
		      testcase_in_top_groups2,
		      testcase_in_top_groups3,
		      testcase_in_top_groups4,
		      testcase_in_top_groups5,
		      testcase_in_top_groups6,
		      testcase_in_top_groups7,
		      testcase_in_sub_groups1,
		      testcase_in_sub_groups2,
		      testcase_in_sub_groups3,
		      testcase_in_sub_groups4,
		      testcase_in_sub_groups5,
		      testcase_in_sub_groups6,
		      testcase_in_sub_groups7,
		      testcase_in_sub_groups8,
		      testcase_in_sub_groups9,
		      testcase_in_sub_groups10,
		      testcase_in_sub_groups11,
		      testcase_in_sub_groups12,
		      testcase_in_sub_groups13,
		      bad_testcase_in_sub_groups1]},

     {run_groups,[sequence],[run_groups_with_options,
			     run_groups_with_testspec]}
    ].

all() ->
    [{group,find_groups,[parallel]},
     {group,run_groups}].



%%--------------------------------------------------------------------
%% TEST CASES CHECKING RETURN VALUE ONLY
%%--------------------------------------------------------------------

all_groups(_) ->
    GPath = all, TCs = all,

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),
    
    Top1 = ct_groups:find_groups(?M1, top1, TCs, groups1()),
    Top2 = ct_groups:find_groups(?M1, top2, TCs, groups1()),
    
    All = Top1 ++ Top2 ++ [{conf,[{name,sub2}],
 			    {?M1,init_per_group},
 			    [{?M1,sub2_tc1},{?M1,sub2_tc2}],
 			    {?M1,end_per_group}}],

    All = Found,

    {?M1,GPath,TCs,Top1++Top2}.

%%%-----------------------------------------------------------------
%%%
testcases_in_all_groups(_) ->
    GPath = all, TCs = [tc3,sub_tc2],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [Top1 =
	 {conf,[{name,top1}],{?M2,init_per_group},
	  [{?M2,tc3},
	   {conf,[{name,sub11}],
	    {?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],
	    {?M2,end_per_group}},
	   {conf,[{name,sub12}],
	    {?M2,init_per_group},
	    [{?M2,tc3},{?M2,sub_tc2},
	     {conf,[{name,sub121}],
	      {?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},
     
     Top2 =
	 {conf,[{name,top2}],{?M2,init_per_group},
	  [{?M2,tc3},
	   {conf,[{name,sub21}],
	    {?M2,init_per_group},
	    [{?M2,tc3},{?M2,sub_tc2},
	     {conf,[{name,sub2xx}],
	      {?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}},
	   
	   {conf,[{name,sub22}],
	    {?M2,init_per_group},
	    [{?M2,tc3},{?M2,sub_tc2},
	     {conf,[{name,sub221}],
	      {?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],
	      {?M2,end_per_group}},
	     {conf,[{name,sub2xx}],
	      {?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},

     {conf,[{name,sub21}],
      {?M2,init_per_group},
      [{?M2,tc3},{?M2,sub_tc2},
       {conf,[{name,sub2xx}],
	{?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,sub22}],
      {?M2,init_per_group},
      [{?M2,tc3},{?M2,sub_tc2},
       {conf,[{name,sub221}],
	{?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],{?M2,end_per_group}},
       {conf,[{name,sub2xx}],
	{?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,sub221}],
      {?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],{?M2,end_per_group}},

     {conf,[{name,sub2xx}],
      {?M2,init_per_group},[{?M2,tc3},{?M2,sub_tc2}],{?M2,end_per_group}}]

	= Found,

    {?M2,GPath,TCs,[Top1,Top2]}.

%%%-----------------------------------------------------------------
%%% 
all_in_top_group1(_) ->
    GPath= top1, TCs = all,

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top1}],
      {?M1,init_per_group},
      [{?M1,top1_tc1},{?M1,top1_tc2},
       {conf,[{name,sub1}],
	{?M1,init_per_group},
	[{?M1,sub1_tc1},{?M1,sub1_tc2}],
	{?M1,end_per_group}}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%% 
all_in_top_group2(_) ->
    GPath= top2, TCs = all,

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top2}],
      {?M1,init_per_group},
      [{conf,[{name,sub2}],
	{?M1,init_per_group},
	[{?M1,sub2_tc1},{?M1,sub2_tc2}],
	{?M1,end_per_group}},
       {?M1,top2_tc1},{?M1,top2_tc2}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%% 
all_in_sub_group1(_) ->
    GPath = sub1, TCs = all,

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top1}],
      {?M1,init_per_group},
      [{conf,[{name,sub1}],
	{?M1,init_per_group},
	[{?M1,sub1_tc1},{?M1,sub1_tc2}],
	{?M1,end_per_group}}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%% 
all_in_sub_group2(_) ->
    GPath = sub2, TCs = all,

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [Top2 =
	 {conf,[{name,top2}],
	  {?M1,init_per_group},
	  [{conf,[{name,sub2}],
	    {?M1,init_per_group},
	    [{?M1,sub2_tc1},{?M1,sub2_tc2}],
	    {?M1,end_per_group}}],
	  {?M1,end_per_group}},
     
     {conf,[{name,sub2}],
      {?M1,init_per_group},
      [{?M1,sub2_tc1},{?M1,sub2_tc2}],
      {?M1,end_per_group}}] = Found,
    
    {?M1,GPath,TCs,Top2}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_group1(_) ->
    GPath = top1, TCs = [top1_tc2],

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top1}],
      {?M1,init_per_group},
      [{?M1,top1_tc2}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_group2(_) ->
    GPath = top2, TCs = [top2_tc2],

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top2}],
      {?M1,init_per_group},
      [{?M1,top2_tc2}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_group1(_) ->
    GPath = sub1, TCs = [sub1_tc2],

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top1}],
      {?M1,init_per_group},
      [{conf,[{name,sub1}],
	{?M1,init_per_group},
	[{?M1,sub1_tc2}],
	{?M1,end_per_group}}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_group2(_) ->
    GPath = sub2, TCs = [sub2_tc2],

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [Top2 =
	 {conf,[{name,top2}],
	  {?M1,init_per_group},
	  [{conf,[{name,sub2}],
	    {?M1,init_per_group},
	    [{?M1,sub2_tc2}],
	    {?M1,end_per_group}}],
	  {?M1,end_per_group}},
     
     {conf,[{name,sub2}],
      {?M1,init_per_group},
      [{?M1,sub2_tc2}],
      {?M1,end_per_group}}] = Found,
    
    {?M1,GPath,TCs,Top2}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups1(_) ->
    GPath = [top1,top2], TCs = all,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{?M2,top1_tc1},{?M2,top_tc2},{?M2,tc3},
       {conf,[{name,sub11}],
	{?M2,init_per_group},
	[{?M2,sub11_tc1},{?M2,sub_tc2},{?M2,tc3}],
	{?M2,end_per_group}},
       {conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,sub12_tc1},{?M2,sub_tc2},{?M2,tc3},
	 {conf,[{name,sub121}],
	  {?M2,init_per_group},
	  [{?M2,sub121_tc1},{?M2,sub_tc2},{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}},
     
     {conf,[{name,top2}],
      {?M2,init_per_group},
      [{conf,[{name,sub21}],
	{?M2,init_per_group},
	[{?M2,sub21_tc1},{?M2,sub_tc2},{?M2,tc3},
	 {conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,sub2xx_tc1},{?M2,sub_tc2},{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}},
       {?M2,top2_tc1},{?M2,top_tc2},{?M2,tc3},
       {conf,[{name,sub22}],
	{?M2,init_per_group},
	[{conf,[{name,sub221}],
	  {?M2,init_per_group},
	  [{?M2,sub221_tc1},{?M2,sub_tc2},{?M2,tc3}],
	  {?M2,end_per_group}},
	 {?M2,sub22_tc1},{?M2,sub_tc2},{?M2,tc3},
	 {conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,sub2xx_tc1},{?M2,sub_tc2},{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups2(_) ->
    GPath = [top1,top2], TCs = tc3,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{?M2,tc3},
       {conf,[{name,sub11}],
	{?M2,init_per_group},
	[{?M2,tc3}],
	{?M2,end_per_group}},
       {conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,tc3},
	 {conf,[{name,sub121}],
	  {?M2,init_per_group},
	  [{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,top2}],
      {?M2,init_per_group},
      [{?M2,tc3},
       {conf,[{name,sub21}],
	{?M2,init_per_group},
	[{?M2,tc3},
	 {conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}},
       
       {conf,[{name,sub22}],
	{?M2,init_per_group},
	[{?M2,tc3},
	 {conf,[{name,sub221}],
	  {?M2,init_per_group},
	  [{?M2,tc3}],
	  {?M2,end_per_group}},	 
	 {conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups3(_) ->
    GPath = [top1,top2], TCs = top1_tc1,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{?M2,top1_tc1}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups4(_) ->
    GPath = [top1,top2], TCs = sub2xx_tc1,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top2}],
      {?M2,init_per_group},
      [{conf,[{name,sub21}],
	{?M2,init_per_group},
	[{conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,sub2xx_tc1}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}},
       {conf,[{name,sub22}],
	{?M2,init_per_group},
	[{conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,sub2xx_tc1}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups5(_) ->
    GPath = [top1,top2], TCs = [sub21_tc1,sub22_tc1],

    Found = ct_groups:find_groups(?M2, [top1,top2], [sub21_tc1,sub22_tc1],
				  groups2()),

    [{conf,[{name,top2}],
      {?M2,init_per_group},
      [{conf,[{name,sub21}],
	{?M2,init_per_group},
	[{?M2,sub21_tc1}],
	{?M2,end_per_group}},
       {conf,[{name,sub22}],
	{?M2,init_per_group},
	[{?M2,sub22_tc1}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups6(_) ->
    GPath = [[top1],[top2]], TCs = tc3,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),
    
    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{?M2,tc3}],
      {?M2,end_per_group}},
     {conf,[{name,top2}],
      {?M2,init_per_group},
      [{?M2,tc3}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups7(_) ->
    GPath = [[top1],[top2]], TCs = all,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{?M2,top1_tc1},
       {?M2,top_tc2},
       {?M2,tc3}],
      {?M2,end_per_group}},
     {conf,[{name,top2}],
      {?M2,init_per_group},
      [{?M2,top2_tc1},
       {?M2,top_tc2},
       {?M2,tc3}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups1(_) ->
    GPath = [sub121], TCs = tc3,

    Found = ct_groups:find_groups(?M2, sub121, tc3, groups2()),
    Found = ct_groups:find_groups(?M2, [sub121], tc3, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{conf,[{name,sub12}],
	{?M2,init_per_group},
	[{conf,[{name,sub121}],
	  {?M2,init_per_group},
	  [{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups2(_) ->
    GPath = sub12, TCs = tc3,

    Found = ct_groups:find_groups(?M2, sub12, tc3, groups2()),
    Found = ct_groups:find_groups(?M2, [sub12], tc3, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,tc3},
	 {conf,[{name,sub121}],
	  {?M2,init_per_group},
	  [{?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    FoundX = ct_groups:find_groups(?M2, [[sub12]], tc3, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,tc3}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = FoundX,
    
    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups3(_) ->
    GPath = [sub121,sub221], TCs = all,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),
    
    [Top1 =
	 {conf,[{name,top1}],
	  {?M2,init_per_group},
	  [{conf,[{name,sub12}],
	    {?M2,init_per_group},
	    [{conf,[{name,sub121}],
	      {?M2,init_per_group},
	      [{?M2,sub121_tc1},
	       {?M2,sub_tc2},
	       {?M2,tc3}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},
     
     Top2 =
	 {conf,[{name,top2}],
	  {?M2,init_per_group},
	  [{conf,[{name,sub22}],
	    {?M2,init_per_group},
	    [{conf,[{name,sub221}],
	      {?M2,init_per_group},
	      [{?M2,sub221_tc1},
	       {?M2,sub_tc2},
	       {?M2,tc3}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},
     
     {conf,[{name,sub22}],
      {?M2,init_per_group},
      [{conf,[{name,sub221}],
	{?M2,init_per_group},
	[{?M2,sub221_tc1},
	 {?M2,sub_tc2},
	 {?M2,tc3}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}},
     
     {conf,[{name,sub221}],
      {?M2,init_per_group},
      [{?M2,sub221_tc1},
       {?M2,sub_tc2},
       {?M2,tc3}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,[Top1,Top2]}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups4(_) ->
    GPath = [top1,sub21], TCs = sub_tc2,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [Top1 =
	 {conf,[{name,top1}],
	  {?M2,init_per_group},
	  [{conf,[{name,sub11}],
	    {?M2,init_per_group},
	    [{?M2,sub_tc2}],
	    {?M2,end_per_group}},
	   {conf,[{name,sub12}],
	    {?M2,init_per_group},
	    [{?M2,sub_tc2},
	     {conf,[{name,sub121}],
	      {?M2,init_per_group},
	      [{?M2,sub_tc2}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},

     Top2 =
	 {conf,[{name,top2}],
	  {?M2,init_per_group},
	  [{conf,[{name,sub21}],
	    {?M2,init_per_group},
	    [{?M2,sub_tc2},
	     {conf,[{name,sub2xx}],
	      {?M2,init_per_group},
	      [{?M2,sub_tc2}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},
     
     {conf,[{name,sub21}],
      {?M2,init_per_group},
      [{?M2,sub_tc2},
       {conf,[{name,sub2xx}],
	{?M2,init_per_group},
	[{?M2,sub_tc2}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,
    
    {?M2,GPath,TCs,[Top1,Top2]}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups5(_) ->
    GPath = [[top1,sub12]], TCs = sub12_tc1,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,sub12_tc1}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups6(_) ->
    GPath = [[top1,sub12]], TCs = [sub_tc2],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,sub_tc2}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups7(_) ->
    GPath = [[top1,sub12]], TCs = [sub12_tc1,sub_tc2],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,sub12_tc1},
	 {?M2,sub_tc2}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups8(_) ->
    GPath = [[top2,sub22]], TCs = [sub22_tc1,sub_tc2],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top2}],
      {?M2,init_per_group},
      [{conf,[{name,sub22}],
	{?M2,init_per_group},
	[{?M2,sub22_tc1},
	 {?M2,sub_tc2}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups9(_) ->
    GPath = [[sub2xx]], TCs = tc3,

    Found = ct_groups:find_groups(?M2, sub2xx, tc3, groups2()),
    Found = ct_groups:find_groups(?M2, [[sub2xx]], tc3, groups2()),

    [Top2 =
	 {conf,[{name,top2}],
	  {?M2,init_per_group},
	  [{conf,[{name,sub21}],
	    {?M2,init_per_group},
	    [{conf,[{name,sub2xx}],
	      {?M2,init_per_group},
	      [{?M2,tc3}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}},
	   {conf,[{name,sub22}],
	    {?M2,init_per_group},
	    [{conf,[{name,sub2xx}],
	      {?M2,init_per_group},
	      [{?M2,tc3}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},
     
     {conf,[{name,sub21}],
      {?M2,init_per_group},
      [{conf,[{name,sub2xx}],
	{?M2,init_per_group},
	[{?M2,tc3}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,sub22}],
      {?M2,init_per_group},
      [{conf,[{name,sub2xx}],
	{?M2,init_per_group},
	[{?M2,tc3}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,sub2xx}],
      {?M2,init_per_group},
      [{?M2,tc3}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Top2}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups10(_) ->
    GPath = [[sub22,sub2xx]], TCs = tc3,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [Top2 = 
	 {conf,[{name,top2}],
	  {?M2,init_per_group},
	  [{conf,[{name,sub22}],
	    {?M2,init_per_group},
	    [{conf,[{name,sub2xx}],
	      {?M2,init_per_group},
	      [{?M2,tc3}],
	      {?M2,end_per_group}}],
	    {?M2,end_per_group}}],
	  {?M2,end_per_group}},

     {conf,[{name,sub22}],
      {?M2,init_per_group},
      [{conf,[{name,sub2xx}],
	{?M2,init_per_group},
	[{?M2,tc3}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Top2}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups11(_) ->
    GPath = [[top1,sub12,sub121]], TCs = all,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],
      {?M2,init_per_group},
      [{conf,[{name,sub12}],
	{?M2,init_per_group},
	[{conf,[{name,sub121}],
	  {?M2,init_per_group},
	  [{?M2,sub121_tc1},
	   {?M2,sub_tc2},
	   {?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups12(_) ->
    GPath = [[top2,sub2xx]], TCs = [sub2xx_tc1,tc3],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top2}],
      {?M2,init_per_group},
      [{conf,[{name,sub21}],
	{?M2,init_per_group},
	[{conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,sub2xx_tc1},
	   {?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}},
       {conf,[{name,sub22}],
	{?M2,init_per_group},
	[{conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,sub2xx_tc1},
	   {?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups13(_) ->
    GPath = [[top2,sub22,sub2xx]], TCs = [top2_tc1,sub2xx_tc1,tc3],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top2}],
      {?M2,init_per_group},
      [{conf,[{name,sub22}],
	{?M2,init_per_group},
	[{conf,[{name,sub2xx}],
	  {?M2,init_per_group},
	  [{?M2,sub2xx_tc1},
	   {?M2,tc3}],
	  {?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}}] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
bad_testcase_in_sub_groups1(_) ->
    GPath = [sub2xx], TCs = [top2_tc1],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%%
bad_testcase_in_sub_groups2(_) ->
    GPath = [sub12,sub2xx], TCs = [top1_tc1,top2_tc1],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [] = Found,

    {?M2,GPath,TCs,Found}.

%%%-----------------------------------------------------------------
%%% CASES EXECUTING THE TESTS
%%%-----------------------------------------------------------------

run_groups_with_options(Config) ->
    DataDir = ?config(data_dir, Config),

    {M1All,M1Rest,M2All,M2Rest} = get_all_groups_and_cases(Config),

    M1AllGrs = lists:flatmap(fun({Path,_,_}) when is_atom(hd(Path)) -> Path;
				({Path,_,_}) when is_list(hd(Path)) -> Path;
				({Path,_,_}) -> [Path]
			     end, M1All),

    %% ct:pal("NOW RUNNING M1 TEST: ~p", [M1All]),

    {OptsM11,ERPidM11} = setup([{dir,DataDir},{suite,?M1},
				{group,M1AllGrs},{label,m1_all_cases}], Config),
    M1AllGrInfo = {M1AllGrs,lists:flatten([Found || {_,_,Found} <- M1All])},
    ok = execute(m1_all_cases, M1AllGrInfo, OptsM11, ERPidM11, Config),

    lists:foldl(
      fun({GrPath,TCs,Found}, N) ->
    	      TestName = list_to_atom("m1_spec_cases_" ++ integer_to_list(N)),
	      %% ct:pal("NOW RUNNING M1 TEST ~p: ~p + ~p",
		%%     [TestName,GrPath,TCs]),
    	      {OptsM12,ERPidM12} = setup([{dir,DataDir},{suite,?M1},
    					  {group,GrPath},{testcase,TCs},
    					  {label,TestName}], Config),
    	      ok = execute(TestName, {GrPath,TCs,Found},
    			   OptsM12, ERPidM12, Config),
    	      N+1
      end, 1, M1Rest),
    
    %% ct:pal("NOW RUNNING M2 TEST: ~p", [M2All]),

    M2AllGrs = lists:flatmap(fun({Path,_,_}) when is_atom(hd(Path)) -> Path;
				({Path,_,_}) when is_list(hd(Path)) -> Path;
				({Path,_,_}) -> [Path]
			     end, M2All),


    {OptsM21,ERPidM21} = setup([{dir,DataDir},{suite,?M2},
    				{group,M2AllGrs},{testcase,all},
    				{label,m2_all_cases}], Config),
    M2AllGrInfo = {M2AllGrs,lists:flatten([Found || {_,_,Found} <- M2All])},
    ok = execute(m2_all_cases, M2AllGrInfo, OptsM21, ERPidM21, Config),

    lists:foldl(
      fun({GrPath,TCs,Found}, N) ->
    	      TestName = list_to_atom("m2_spec_cases_" ++ integer_to_list(N)),
           %% ct:pal("NOW RUNNING M2 TEST ~p: ~p + ~p", [TestName,GrPath,TCs]),
    	      {OptsM22,ERPidM22} = setup([{dir,DataDir},{suite,?M2},
    					  {group,GrPath},{testcase,TCs},
    					  {label,TestName}], Config),
    	      ok = execute(TestName, {GrPath,TCs,Found},
    			   OptsM22, ERPidM22, Config),
    	      N+1
      end, 1, M2Rest),
    ok.


%%%-----------------------------------------------------------------
%%% 
run_groups_with_testspec(Config) ->
    Name = run_groups_with_testspec,
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    {M1All,M1Rest,M2All,M2Rest} = get_all_groups_and_cases(Config),

    M1AllGrs = lists:flatmap(fun({Path,_,_}) when is_atom(hd(Path)) -> Path;
				({Path,_,_}) when is_list(hd(Path)) -> Path;
				({Path,_,_}) -> [Path]
			     end, M1All),
    M1AllTerm =	{groups,DataDir,?M1,M1AllGrs},

    M1RestTerms = lists:map(
		    fun({GrPath,TCs,_}) ->
			    {groups,DataDir,?M1,GrPath,{cases,TCs}}
		    end, M1Rest),

    M2AllGrs = lists:flatmap(fun({Path,_,_}) when is_atom(hd(Path)) -> Path;
				({Path,_,_}) when is_list(hd(Path)) -> Path;
				({Path,_,_}) -> [Path]
			     end, M2All),
    M2AllTerm =	{groups,DataDir,?M2,M2AllGrs,{cases,all}},

    M2RestTerms = lists:map(
		    fun({GrPath,TCs,_}) ->
			    {groups,DataDir,?M2,GrPath,{cases,TCs}}
		    end, M2Rest),

    GroupTerms = lists:flatten([M1AllTerm,
				M1RestTerms,
				M2AllTerm,
				M2RestTerms]),

    TestSpec = [{merge_tests,false},
		{label,Name}] ++ GroupTerms,

    ct:pal("Here's the test spec:~n~p", [TestSpec]),

    TestSpecName = ct_test_support:write_testspec(TestSpec, PrivDir,
						  "groups_search_spec"),

    {Opts,ERPid} = setup([{spec,TestSpecName}], Config),
    GroupInfo = 
	[{M1AllTerm,lists:flatten([Found || {_,_,Found} <- M1All])} |
	 M1Rest] ++
	[{M2AllTerm,lists:flatten([Found || {_,_,Found} <- M2All])} |
	 M2Rest],
    ok = execute(Name, GroupInfo, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

groups1() ->
    [{top1,[],[top1_tc1,top1_tc2,{sub1,[],[sub1_tc1,sub1_tc2]}]},
     {top2,[],[{group,sub2},top2_tc1,top2_tc2]},
     {sub2,[],[sub2_tc1,sub2_tc2]}].

groups2() ->
    [{top1,[],[top1_tc1,top_tc2,tc3,
	       {sub11,[],[sub11_tc1,sub_tc2,tc3]},
	       {sub12,[],[sub12_tc1,sub_tc2,tc3,
			  {sub121,[],[sub121_tc1,sub_tc2,tc3]}]}]},
     {top2,[],[{group,sub21},top2_tc1,top_tc2,tc3,{group,sub22}]},
     {sub21,[],[sub21_tc1,sub_tc2,tc3,{group,sub2xx}]},
     {sub22,[],[{group,sub221},sub22_tc1,sub_tc2,tc3,{group,sub2xx}]},
     {sub221,[],[sub221_tc1,sub_tc2,tc3]},
     {sub2xx,[],[sub2xx_tc1,sub_tc2,tc3]}].

get_all_groups_and_cases(Config) ->
    {value,{_,_,FindGrTCs}} = lists:keysearch(find_groups, 1, groups()),

    MGTFs = [apply(?MODULE, TC, [Config]) || TC <- FindGrTCs],

    ct:pal("Extracted data from ~p test cases", [length(MGTFs)]),

    lists:foldr(fun({M,Gs,TCs,F},
		    {M11,M12,M21,M22}) ->
			case {M,Gs,TCs} of
			    {?M1,all,_} -> {M11,[{Gs,TCs,F}|M12],M21,M22};
			    {?M1,_,all} -> {[{Gs,all,F}|M11],M12,M21,M22};
			    {?M1,_,_} -> {M11,[{Gs,TCs,F}|M12],M21,M22};
			    {?M2,all,_} -> {M11,M12,M21,[{Gs,TCs,F}|M22]};
			    {?M2,_,all} -> {M11,M12,[{Gs,all,F}|M21],M22};
			    {?M2,_,_} -> {M11,M12,M21,[{Gs,TCs,F}|M22]}
			end
		end, {[],[],[],[]}, MGTFs).
    
%%%-----------------------------------------------------------------

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}}|Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

execute(Name, TestParams, Opts, ERPid, Config) ->
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),
    Events1 = reformat(Events, ?eh),
    ct_test_support:log_events(Name, 
			       Events1,
			       ?config(priv_dir, Config),
			       Opts),
    verify_events(Name, TestParams, Events1).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
verify_events(Name, Params, Events) ->
    %% 2 tests (ct:run_test + script_start) is default
    verify_events(Name, Params, Events, 2).

verify_events(_, _, _, 0) ->
    ok;
verify_events(Name, Params, Events, N) ->
    test_events(Name, Params, Events),
    verify_events(Name, Params, Events, N-1).

%%%-----------------------------------------------------------------
%%% check run_groups_with_options

test_events(TestName, {GrPath,Found}, Events) ->
    test_events(TestName, {GrPath,all,Found}, Events);

test_events(TestName, {GrPath,TCs,Found}, Events)
  when TestName /= run_groups_with_testspec ->
    try check_events(Events, flatten_tests(Found)) of
	ok -> ok
    catch
	throw:Reason ->
	    ct:pal("Test failed for ~p with group path ~p and cases ~p"
		   "~nReason: ~p", [TestName,GrPath,TCs,Reason]),
	    throw(failed)
    end;

%%%-----------------------------------------------------------------
%%% check run_groups_with_testspec

test_events(run_groups_with_testspec, Params, Events) ->
    AllFound = lists:flatmap(fun({_All,Found}) when is_tuple(Found) ->
				     [Found];
				({_All,Found}) -> 
				     Found;
				({_Gr,_TCs,Found}) when is_tuple(Found) ->
				     [Found];
				({_Gr,_TCs,Found}) ->
				     Found
			     end, Params),
    try check_events(Events, flatten_tests(AllFound)) of
	ok -> ok
    catch
	throw:Reason ->
	    ct:pal("Test failed for run_groups_with_testspec."
		   "~nReason: ~p", [Reason]),
	    throw(failed)
    end.

flatten_tests({conf,[{name,G}|_],{Mod,_I},Tests,_E}) ->
    lists:flatten([{group,Mod,G} | flatten_tests(Tests)]);
flatten_tests([{conf,[{name,G}|_],{Mod,_I},Tests,_E} | Confs]) ->
    lists:flatten([{group,Mod,G} | flatten_tests(Tests)]) ++
	lists:flatten(flatten_tests(Confs));
flatten_tests([{_Mod,_TC} = Case | Tests]) ->
    lists:flatten([Case | flatten_tests(Tests)]); 
flatten_tests([]) ->
    [].

check_events([{_,tc_start,{Mod,{init_per_group,G,_}}} | Evs],
	     [{group,Mod,G} | Check]) ->
    check_events(Evs, Check);
check_events([{_,tc_start,{Mod,TC}} | Evs],
	     [{Mod,TC} | Check]) when is_atom(TC) ->
    check_events(Evs, Check);
check_events([{_,tc_start,{Mod,{init_per_group,G,_}}} | _Evs], Check) ->
    ct:pal("CHECK FAILED!~nGroup ~p in ~p not found in ~p.",
	   [G,Mod,Check]),
    throw({test_not_found,{Mod,G}});
check_events([{_,tc_start,{Mod,TC}} | _Evs], Check)
  when is_atom(TC), TC /= init_per_suite, TC /= end_per_suite ->
    ct:pal("CHECK FAILED!~nCase ~p in ~p not found in ~p.",
	   [TC,Mod,Check]),
    throw({test_not_found,{Mod,TC}});
check_events([Group | Evs], Check) when is_list(Group) ->
    Check1 = check_events(Group, Check),
    check_events(Evs, Check1);
check_events(_, []) ->
    ok;
check_events([Elem | Evs], Check) when is_tuple(Elem) ->
    check_events(Evs, Check);
check_events([], Check = [_|_]) ->
    ct:pal("CHECK FAILED!~nTests remain: ~p", [Check]),
    throw({tests_remain,Check});
check_events([Wut | _],_) ->
    throw({unexpected,Wut}).

