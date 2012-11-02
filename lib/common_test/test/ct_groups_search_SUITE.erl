%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2012. All Rights Reserved.
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

     {run_groups,[],[run_groups_with_options,
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

    {?M1,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcases_in_all_groups(_) ->
    GPath = all, TCs = [sub_tc2,tc3],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top1}],{?M2,init_per_group},
      [{?M2,tc3},
       {conf,[{name,sub11}],
	{?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}},
       {conf,[{name,sub12}],
	{?M2,init_per_group},
	[{?M2,sub_tc2},{?M2,tc3},
	 {conf,[{name,sub121}],
	  {?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,top2}],{?M2,init_per_group},
      [{conf,[{name,sub21}],
	{?M2,init_per_group},
	[{?M2,sub_tc2},{?M2,tc3},
	 {conf,[{name,sub2xx}],
	  {?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}}],
	{?M2,end_per_group}},
       {?M2,tc3},				% in top2
       {conf,[{name,sub22}],
	{?M2,init_per_group},
	[{conf,[{name,sub221}],
	  {?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}},
	 {?M2,sub_tc2},{?M2,tc3},		% in sub22
	 {conf,[{name,sub2xx}],
	  {?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}}],
	{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,sub21}],
      {?M2,init_per_group},
      [{?M2,sub_tc2},{?M2,tc3},
       {conf,[{name,sub2xx}],
	{?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,sub22}],
      {?M2,init_per_group},
      [{conf,[{name,sub221}],
	{?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}},
       {?M2,sub_tc2},{?M2,tc3},
       {conf,[{name,sub2xx}],
	{?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}}],
      {?M2,end_per_group}},

     {conf,[{name,sub221}],
      {?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}},

     {conf,[{name,sub2xx}],
      {?M2,init_per_group},[{?M2,sub_tc2},{?M2,tc3}],{?M2,end_per_group}}]

	= Found,

    {?M2,GPath,TCs}.

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

    {?M1,GPath,TCs}.

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

    {?M1,GPath,TCs}.

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

    {?M1,GPath,TCs}.

%%%-----------------------------------------------------------------
%%% 
all_in_sub_group2(_) ->
    GPath = sub2, TCs = all,

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top2}],
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

    {?M1,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_group1(_) ->
    GPath = top1, TCs = [top1_tc2],

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top1}],
      {?M1,init_per_group},
      [{?M1,top1_tc2}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_group2(_) ->
    GPath = top2, TCs = [top2_tc2],

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top2}],
      {?M1,init_per_group},
      [{?M1,top2_tc2}],
      {?M1,end_per_group}}] = Found,

    {?M1,GPath,TCs}.

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

    {?M1,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_group2(_) ->
    GPath = sub2, TCs = [sub2_tc2],

    Found = ct_groups:find_groups(?M1, GPath, TCs, groups1()),

    [{conf,[{name,top2}],
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

    {?M1,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups1(_) ->
    GPath = [top1,top2], TCs = all,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [
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
       {?M2,end_per_group}}],

     [{conf,[{name,top2}],
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
       {?M2,end_per_group}}]

    ] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups2(_) ->
    GPath = [top1,top2], TCs = tc3,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [
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
       {?M2,end_per_group}}],

     [{conf,[{name,top2}],
       {?M2,init_per_group},
       [{conf,[{name,sub21}],
	 {?M2,init_per_group},
	 [{?M2,tc3},
	  {conf,[{name,sub2xx}],
	   {?M2,init_per_group},
	   [{?M2,tc3}],
	   {?M2,end_per_group}}],
	 {?M2,end_per_group}},
	{?M2,tc3},
	{conf,[{name,sub22}],
	 {?M2,init_per_group},
	 [{conf,[{name,sub221}],
	   {?M2,init_per_group},
	   [{?M2,tc3}],
	   {?M2,end_per_group}},
	  {?M2,tc3},
	  {conf,[{name,sub2xx}],
	   {?M2,init_per_group},
	   [{?M2,tc3}],
	   {?M2,end_per_group}}],
	 {?M2,end_per_group}}],
       {?M2,end_per_group}}]
    ] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups3(_) ->
    GPath = [top1,top2], TCs = top1_tc1,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [[{conf,[{name,top1}],
       {?M2,init_per_group},
       [{?M2,top1_tc1}],
       {?M2,end_per_group}}], []] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups4(_) ->
    GPath = [top1,top2], TCs = sub2xx_tc1,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [[],
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
       {?M2,end_per_group}}]] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups5(_) ->
    GPath = [top1,top2], TCs = [sub21_tc1,sub22_tc1],

    Found = ct_groups:find_groups(?M2, [top1,top2], [sub21_tc1,sub22_tc1],
				  groups2()),

    [[],
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
       {?M2,end_per_group}}]] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups6(_) ->
    GPath = [[top1],[top2]], TCs = tc3,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [[{conf,[{name,top1}],
       {?M2,init_per_group},
       [{?M2,tc3}],
       {?M2,end_per_group}}],
     [{conf,[{name,top2}],
       {?M2,init_per_group},
       [{?M2,tc3}],
       {?M2,end_per_group}}]] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_top_groups7(_) ->
    GPath = [[top1],[top2]], TCs = all,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [[{conf,[{name,top1}],
       {?M2,init_per_group},
       [{?M2,top1_tc1},
	{?M2,top_tc2},
	{?M2,tc3}],
       {?M2,end_per_group}}],
     [{conf,[{name,top2}],
       {?M2,init_per_group},
       [{?M2,top2_tc1},
	{?M2,top_tc2},
	{?M2,tc3}],
       {?M2,end_per_group}}]] = Found,

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

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
    
    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups3(_) ->
    GPath = [sub121,sub221], TCs = all,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [
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
       {?M2,end_per_group}}],

     [{conf,[{name,top2}],
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
       {?M2,end_per_group}}]

    ] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups4(_) ->
    GPath = [top1,sub21], TCs = sub_tc2,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [
     [{conf,[{name,top1}],
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
       {?M2,end_per_group}}],

     [{conf,[{name,top2}],
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
       {?M2,end_per_group}}]

    ] = Found,

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups9(_) ->
    GPath = [[sub2xx]], TCs = tc3,

    Found = ct_groups:find_groups(?M2, sub2xx, tc3, groups2()),
    Found = ct_groups:find_groups(?M2, [[sub2xx]], tc3, groups2()),

    [{conf,[{name,top2}],
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

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
testcase_in_sub_groups10(_) ->
    GPath = [[sub22,sub2xx]], TCs = tc3,

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [{conf,[{name,top2}],
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

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

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

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
bad_testcase_in_sub_groups1(_) ->
    GPath = [sub2xx], TCs = [top2_tc1],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%%
bad_testcase_in_sub_groups2(_) ->
    GPath = [sub12,sub2xx], TCs = [top1_tc1,top2_tc1],

    Found = ct_groups:find_groups(?M2, GPath, TCs, groups2()),

    [[],[]] = Found,

    {?M2,GPath,TCs}.

%%%-----------------------------------------------------------------
%%% CASES EXECUTING THE TESTS
%%%-----------------------------------------------------------------

run_groups_with_options(Config) ->
    DataDir = ?config(data_dir, Config),

    {M1All,M1Rest,M2All,M2Rest} = get_all_groups_and_cases(Config),

    ct:pal("NOW RUNNING M1 TEST: ~p", [M1All]),

    M1AllGrSpec = {group, [Path || {Path,_} <- M1All]},
    {OptsM11,ERPidM11} = setup([{dir,DataDir},{suite,?M1},
				M1AllGrSpec,{label,m1_all_cases}], Config),
    ok = execute(m1_all_cases, M1AllGrSpec, OptsM11, ERPidM11, Config),

    ct:pal("NOW RUNNING M1 TESTS: ~p", [M1Rest]),

    lists:foldl(
      fun({GrPath,TCs}, N) ->
	      TestName = list_to_atom("m1_spec_cases_" ++ integer_to_list(N)),
	      {OptsM12,ERPidM12} = setup([{dir,DataDir},{suite,?M1},
					  {group,GrPath},{testcases,TCs},
					  {label,TestName}], Config),
	      ok = execute(TestName, {{group,GrPath},{testcases,TCs}},
			   OptsM12, ERPidM12, Config),
	      N+1
      end, 1, M1Rest),
    
    ct:pal("NOW RUNNING M2 TEST: ~p", [M2All]),

    M2AllGrSpec = {group, [Path || {Path,_} <- M2All]},
    {OptsM21,ERPidM21} = setup([{dir,DataDir},{suite,?M2},
				M2AllGrSpec,{testcases,all},
				{label,m2_all_cases}], Config),
    ok = execute(m2_all_cases, M2AllGrSpec, OptsM21, ERPidM21, Config),

    ct:pal("NOW RUNNING M2 TESTS: ~p", [M2Rest]),

    lists:foldl(
      fun({GrPath,TCs}, N) ->
	      TestName = list_to_atom("m2_spec_cases_" ++ integer_to_list(N)),
	      {OptsM22,ERPidM22} = setup([{dir,DataDir},{suite,?M2},
					  {group,GrPath},{testcases,TCs},
					  {label,TestName}], Config),
	      ok = execute(TestName, {{group,GrPath},{testcases,TCs}},
			   OptsM22, ERPidM22, Config),
	      N+1
      end, 1, M2Rest),
    ok.


%%%-----------------------------------------------------------------
%%% 
run_groups_with_testspec(Config) ->
    TC = run_groups_with_testspec,
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    {M1All,M1Rest,M2All,M2Rest} = get_all_groups_and_cases(Config),

    M1AllTerm =	{groups,DataDir,?M1,[Path || {Path,_} <- M1All]},
    M1RestTerms = lists:map(
		    fun({GrPath,TCs}) ->
			    {groups,DataDir,?M1,GrPath,{cases,TCs}}
		    end, M1Rest),
    M2AllTerm =	{groups,DataDir,?M2,[Path || {Path,_} <- M2All],{cases,all}},
    M2RestTerms = lists:map(
		    fun({GrPath,TCs}) ->
			    {groups,DataDir,?M2,GrPath,{cases,TCs}}
		    end, M2Rest),

    GroupTerms = lists:flatten([M1AllTerm,
				M1RestTerms,
				M2AllTerm,
				M2RestTerms]),

    TestSpec = [{merge_tests,false},
		{label,TC}] ++ GroupTerms,

    ct:pal("Here's the test spec:~n~p", [TestSpec]),

    TestSpecName = ct_test_support:write_testspec(TestSpec, PrivDir,
						  "groups_search_spec"),

    {Opts,ERPid} = setup([{spec,TestSpecName}], Config),
    ok = execute(TC, GroupTerms, Opts, ERPid, Config).

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

    MGTs = [apply(?MODULE, TC, [Config]) || TC <- FindGrTCs],

    ct:pal("Extracted data from ~p test cases", [length(MGTs)]),

    lists:foldr(fun({M,Gs,TCs},
		    {M11,M12,M21,M22}) ->
			case {M,TCs} of
			    {?M1,all} -> {[{Gs,all}|M11],M12,M21,M22};
			    {?M1,_} -> {M11,[{Gs,TCs}|M12],M21,M22};
			    {?M2,all} -> {M11,M12,[{Gs,all}|M21],M22};
			    {?M2,_} -> {M11,M12,M21,[{Gs,TCs}|M22]}
			end
		end, {[],[],[],[]}, MGTs).
    
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

    ct_test_support:log_events(Name, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(Name, TestParams),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
events_to_check(Test, Params) ->
    %% 2 tests (ct:run_test + script_start) is default
    events_to_check(Test, Params, 2).

events_to_check(_, _, 0) ->
    [];
events_to_check(Test, Params, N) ->
    test_events(Test, Params) ++ events_to_check(Test, Params, N-1).

%%%-----------------------------------------------------------------
%%% check run_groups_with_options

test_events(m1_all_cases, {_,[all,top1,top2,sub1,sub2]}) ->
    [];

test_events(m1_spec_cases_1, {{_,top1},{_,[top1_tc2]}}) ->
    [];
test_events(m1_spec_cases_2, {{_,top2},{_,[top2_tc2]}}) ->
    [];
test_events(m1_spec_cases_3, {{_,sub1},{_,[sub1_tc2]}}) ->
    [];
test_events(m1_spec_cases_4, {{_,sub2},{_,[sub2_tc2]}}) ->
    [];
test_events(m1_spec_cases_5, {{_,[top1,top2]},{_,[sub21_tc1,sub22_tc1]}}) ->
    [];
test_events(m1_spec_cases_6, {{_,[[top1],[top2]]},{_,tc3}}) ->
    [];

test_events(m2_all_cases, {_,[[top1,top2],
				  [[top1],[top2]],
				  [sub121,sub221],
				  [[top1,sub12,sub121]]]}) ->
    [];

test_events(m2_spec_cases_1, {{_,all},{_,[sub_tc2,tc3]}}) ->
    [];
test_events(m2_spec_cases_2, {{_,[top1,top2]},{_,tc3}}) ->
    [];
test_events(m2_spec_cases_3, {{_,[top1,top2]},{_,top1_tc1}}) ->
    [];
test_events(m2_spec_cases_4, {{_,[top1,top2]},{_,sub2xx_tc1}}) ->
    [];
test_events(m2_spec_cases_5, {{_,[top1,top2]},{_,[sub21_tc1,sub22_tc1]}}) ->
    [];
test_events(m2_spec_cases_6, {{_,[[top1],[top2]]},{_,tc3}}) ->
    [];
test_events(m2_spec_cases_7, {{_,[sub121]},{_,tc3}}) ->
    [];
test_events(m2_spec_cases_8, {{_,sub12},{_,tc3}}) ->
    [];
test_events(m2_spec_cases_9, {{_,[top1,sub21]},{_,sub_tc2}}) ->
    [];
test_events(m2_spec_cases_10, {{_,[[top1,sub12]]},{_,sub12_tc1}}) ->
    [];
test_events(m2_spec_cases_11, {{_,[[top1,sub12]]},{_,[sub_tc2]}}) ->
    [];
test_events(m2_spec_cases_12, {{_,[[top1,sub12]]},{_,[sub12_tc1,sub_tc2]}}) ->
    [];
test_events(m2_spec_cases_13, {{_,[[top2,sub22]]},{_,[sub22_tc1,sub_tc2]}}) ->
    [];
test_events(m2_spec_cases_14, {{_,[[sub2xx]]},{_,tc3}}) ->
    [];
test_events(m2_spec_cases_15, {{_,[[sub22,sub2xx]]},{_,tc3}}) ->
    [];
test_events(m2_spec_cases_16, {{_,[[top2,sub2xx]]},{_,[sub2xx_tc1,tc3]}}) ->
    [];
test_events(m2_spec_cases_17, {{_,[[top2,sub22,sub2xx]]},
			       {_,[top2_tc1,sub2xx_tc1,tc3]}}) ->
    [];
test_events(m2_spec_cases_18, {{_,[sub2xx]},{_,[top2_tc1]}}) ->
    [];

%%%-----------------------------------------------------------------
%%% check run_groups_with_testspec

test_events(run_groups_with_testspec, 
	    [{_,_,_,[all,top1,top2,sub1,sub2]},
	     {_,_,_,top1,{cases,[top1_tc2]}},
	     {_,_,_,top2,{cases,[top2_tc2]}},
	     {_,_,_,sub1,{cases,[sub1_tc2]}},
	     {_,_,_,sub2,{cases,[sub2_tc2]}},
	     {_,_,_,[[top1,top2],[[top1],[top2]],
		     [sub121,sub221],[[top1,sub12,sub121]]],{cases,all}},
	     {_,_,_,all,{cases,[sub_tc2,tc3]}},
	     {_,_,_,[top1,top2],{cases,tc3}},
	     {_,_,_,[top1,top2],{cases,top1_tc1}},
	     {_,_,_,[top1,top2],{cases,sub2xx_tc1}},
	     {_,_,_,[top1,top2],{cases,[sub21_tc1,sub22_tc1]}},
	     {_,_,_,[[top1],[top2]],{cases,tc3}},
	     {_,_,_,[sub121],{cases,tc3}},
	     {_,_,_,sub12,{cases,tc3}},
	     {_,_,_,[top1,sub21],{cases,sub_tc2}},
	     {_,_,_,[[top1,sub12]],{cases,sub12_tc1}},
	     {_,_,_,[[top1,sub12]],{cases,[sub_tc2]}},
	     {_,_,_,[[top1,sub12]],{cases,[sub12_tc1,sub_tc2]}},
	     {_,_,_,[[top2,sub22]],{cases,[sub22_tc1,sub_tc2]}},
	     {_,_,_,[[sub2xx]],{cases,tc3}},
	     {_,_,_,[[sub22,sub2xx]],{cases,tc3}},
	     {_,_,_,[[top2,sub2xx]],{cases,[sub2xx_tc1,tc3]}},
	     {_,_,_,[[top2,sub22,sub2xx]],{cases,[top2_tc1,sub2xx_tc1,tc3]}},
	     {_,_,_,[sub2xx],{cases,[top2_tc1]}}]) ->
    [].



