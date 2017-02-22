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
%%% File: ct_testspec_2_SUITE
%%%
%%% Description:
%%% Test test specifications
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_testspec_2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/src/ct_util.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic_compatible_no_nodes,
     basic_compatible_nodes,
     unknown_terms,
     no_merging,
     multiple_specs,
     misc_config_terms,
     define_names_1].


%%--------------------------------------------------------------------
%% VALID TEST SPEC TERMS (R15B02):
%%
%% {node,3}
%% {cover,2}
%% {cover,3}
%% {config,2}
%% {config,3}
%% {config,4}
%% {userconfig,2}
%% {userconfig,3}
%% {alias,3}
%% {merge_tests,2}
%% {logdir,2}
%% {logdir,3}
%% {logopts,2}
%% {logopts,3}
%% {basic_html,2}
%% {basic_html,3}
%% {verbosity,2}
%% {verbosity,3}
%% {silent_connections,2}
%% {silent_connections,3}
%% {label,2}
%% {label,3}
%% {event_handler,2}
%% {event_handler,3}
%% {event_handler,4}
%% {ct_hooks,2}
%% {ct_hooks,3}
%% {enable_builtin_hooks,2}
%% {release_shell,2}
%% {multiply_timetraps,2}
%% {multiply_timetraps,3}
%% {scale_timetraps,2}
%% {scale_timetraps,3}
%% {include,2}
%% {include,3}
%% {auto_compile,2}
%% {auto_compile,3}
%% {stylesheet,2}
%% {stylesheet,3}
%% {suites,3}
%% {suites,4}
%% {groups,4}
%% {groups,5}
%% {groups,6}
%% {cases,4}
%% {cases,5}
%% {skip_suites,4}
%% {skip_suites,5}
%% {skip_groups,5}
%% {skip_groups,6}
%% {skip_groups,7}
%% {skip_cases,5}
%% {skip_cases,6}
%% {create_priv_dir,2}
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
basic_compatible_no_nodes(_Config) ->

    AliasDir1 = "../tests/to1",
    AliasDir2 = "../tests/to2",
    CfgDir1 = "../cfgs/to1/x.cfg",
    CfgDir2 = ["../cfgs/to2/x.cfg","../cfgs/to2/y.cfg"],
    LogDir = "../logs",
    IncludeDir1 = "../../include",
    IncludeDir2 = ["../tests/to1/include","../tests/to2/include"],

    Spec =
	[
	 {label,"basic_compatible_no_nodes"},
	 {alias,to1,AliasDir1},
	 {alias,to2,AliasDir2},
	 {config,CfgDir1},
	 {config,CfgDir2},
	 {userconfig,{?MODULE,"cfg_str1"}},
	 {userconfig,{?MODULE,"cfg_str2"}},
	 {logdir,LogDir},
	 {logopts,[no_nl]},
	 {event_handler,evh1,[1]},
	 {event_handler,[evh2,evh3],[[2,3]]},
	 {ct_hooks,[{cth_mod1,[]}]},
	 {ct_hooks,[{cth_mod2,[]}]},
	 {multiply_timetraps,2},
	 {include,IncludeDir1},
	 {include,IncludeDir2},
	 {suites,to1,[x_SUITE]},
	 {groups,to1,y_SUITE,[g1,g2]},
	 {cases,to1,y_SUITE,[tc1,tc2]},
	 {skip_suites,to1,z_SUITE,"skipped"},
	 {suites,to2,[x_SUITE,y_SUITE]},
	 {skip_groups,to2,x_SUITE,[g1,g2],"skipped"},
	 {skip_cases,to2,y_SUITE,[tc1,tc2],"skipped"}
	],
    
    {ok,SpecDir} = file:get_cwd(),
    
    ListResult = ct_testspec:collect_tests_from_list(Spec, false),
    ct:pal("TESTSPEC RECORD FROM LIST:~n~p~n", [rec2proplist(ListResult)]),	
    SpecFile = ct_test_support:write_testspec(Spec,SpecDir,
					      "basic_compatible_no_nodes.spec"),
    FileResult = ct_testspec:collect_tests_from_file([SpecFile], false),
    ct:pal("TESTSPEC RECORD FROM FILE:~n~p~n", [rec2proplist(FileResult)]),
    
    Node = node(),
    LogDirV = get_absdir(filename:join(SpecDir,"../logs")),
    Alias1V = get_absdir(filename:join(SpecDir,AliasDir1)),
    Alias2V = get_absdir(filename:join(SpecDir,AliasDir2)),
    CFGs = [{Node,get_absdir(filename:join(SpecDir,CfgDir))} ||
	       CfgDir <- [CfgDir1 | CfgDir2]],
    Incls = [{Node,get_absdir(filename:join(SpecDir,IncludeDir))} ||
		IncludeDir <- [IncludeDir1 | IncludeDir2]],
    
    Verify = #testspec{spec_dir = SpecDir,
		       nodes = [{undefined,Node}],
		       init = [],
		       label = [{Node,"basic_compatible_no_nodes"}],
		       logdir = [{Node,LogDirV},"."],
		       logopts = [{Node,[no_nl]}],
		       basic_html = [],
		       cover = [],
		       config = CFGs,
		       userconfig = [{Node,{?MODULE,"cfg_str1"}},
				     {Node,{?MODULE,"cfg_str2"}}],
		       event_handler = [{Node,evh1,[1]},
					{Node,evh2,[[2,3]]},
					{Node,evh3,[[2,3]]}],
		       ct_hooks = [{Node,{cth_mod1,[]}},
				   {Node,{cth_mod2,[]}}],
		       enable_builtin_hooks = true,
		       release_shell = false,
		       include = Incls,
		       auto_compile = [],
		       stylesheet = [],
		       multiply_timetraps = [{Node,2}],
		       scale_timetraps = [],
		       create_priv_dir = [],
		       alias = [{to1,Alias1V},{to2,Alias2V}],			
		       tests = [{{Node,Alias1V},
				 [{x_SUITE,[all]},
				  {y_SUITE,[{g1,all},{g2,all},tc1,tc2]},
				  {z_SUITE,[{all,{skip,"skipped"}}]}]},
				{{Node,Alias2V},
				 [{x_SUITE,[all,
					    {{g1,all},{skip,"skipped"}},
					    {{g2,all},{skip,"skipped"}}]},
				  {y_SUITE,[all,
					    {tc1,{skip,"skipped"}},
					    {tc2,{skip,"skipped"}}]}]}],
		       merge_tests = true},

    verify_result(Verify,ListResult,FileResult),

    {ok,Tests} = ct_testspec:get_tests([SpecFile]),
    ct:pal("ct_testspec:get_tests/1:~n~p~n", [Tests]),
    [{[SpecFile],[{Node,Run,Skip}]}] = Tests,
    [{Alias1V,x_SUITE,all},
     {Alias1V,y_SUITE,[{g1,all},{g2,all},tc1,tc2]},
     {Alias1V,z_SUITE,all},
     {Alias2V,x_SUITE,all},
     {Alias2V,y_SUITE,all}] = lists:sort(Run),
    [{Alias1V,z_SUITE,"skipped"},
     {Alias2V,x_SUITE,{g1,all},"skipped"},
     {Alias2V,x_SUITE,{g2,all},"skipped"},
     {Alias2V,y_SUITE,tc1,"skipped"},
     {Alias2V,y_SUITE,tc2,"skipped"}] = lists:sort(Skip),

    ok.


%%%-----------------------------------------------------------------
%%%
basic_compatible_nodes(_Config) ->

    Node1 = node1@host1,
    Node2 = node2@host2,
    TODir1 = "../tests/to1",
    TODir2 = "../tests/to2",
    CfgDir1 = "../cfgs/to1/x.cfg",
    CfgDir2 = ["../cfgs/to2/x.cfg","../cfgs/to2/y.cfg"],
    LogDir = "../logs",
    MasterLogDir = "../master_logs",
    IncludeDir1 = "../../include",
    IncludeDir2 = ["../tests/to1/include","../tests/to2/include"],

    Spec =
	[
	 {node,n1,Node1},
	 {node,n2,Node2},
	 {init,[n1],[{node_start,[{callback_module,cbm}]}]},
	 {init,n2,[{node_start,[]}]},
	 {init,all_nodes,{eval,{mod,func,[]}}},
	 {label,"basic_compatible_nodes"},
	 {label,n1,basic_compatible_nodes_1},
	 {config,n1,CfgDir1},
	 {config,n2,CfgDir2},
	 {userconfig,{?MODULE,"cfg_str1"}},
	 {userconfig,{?MODULE,"cfg_str2"}},
	 {logdir,all_nodes,LogDir},
	 {logdir,master,MasterLogDir},
	 {logopts,node2@host2,[no_nl]},
	 {event_handler,master,evh1,[1]},
	 {event_handler,[n1,n2],[evh2,evh3],[[2,3]]},
	 {ct_hooks,all_nodes,[{cth_mod1,[]}]},
	 {ct_hooks,[{cth_mod2,[]}]},
	 {multiply_timetraps,node1@host1,2},
	 {include,n1,IncludeDir1},
	 {include,[n1,n2],IncludeDir2},
	 {suites,n1,TODir1,[x_SUITE]},
	 {groups,n1,TODir1,y_SUITE,[g1,g2]},
	 {cases,n1,TODir1,y_SUITE,[tc1,tc2]},
	 {skip_suites,n1,TODir1,z_SUITE,"skipped"},
	 {suites,n2,TODir2,[x_SUITE,y_SUITE]},
	 {skip_groups,n2,TODir2,x_SUITE,[g1,g2],"skipped"},
	 {skip_cases,n2,TODir2,y_SUITE,[tc1,tc2],"skipped"}
	],
    
    {ok,SpecDir} = file:get_cwd(),
    
    ListResult = ct_testspec:collect_tests_from_list(Spec, false),
    ct:pal("TESTSPEC RECORD FROM LIST:~n~p~n", [rec2proplist(ListResult)]),	
    SpecFile = ct_test_support:write_testspec(Spec,SpecDir,
					      "basic_compatible_nodes.spec"),
    FileResult = ct_testspec:collect_tests_from_file([SpecFile], false),
    ct:pal("TESTSPEC RECORD FROM FILE:~n~p~n", [rec2proplist(FileResult)]),
    
    Node = node(),
    LogDirV = get_absdir(filename:join(SpecDir,"../logs")),
    MasterLogDirV = get_absdir(filename:join(SpecDir,"../master_logs")),
    TO1V = get_absdir(filename:join(SpecDir,TODir1)),
    TO2V = get_absdir(filename:join(SpecDir,TODir2)),
    CFGs = [{Node1,get_absdir(filename:join(SpecDir,CfgDir1))} |
	    [{Node2,get_absdir(filename:join(SpecDir,CfgDir))} || CfgDir <- CfgDir2]],
    Incls = [{Node1,get_absdir(filename:join(SpecDir,IncludeDir1))} |
	     [{Node1,get_absdir(filename:join(SpecDir,IncludeDir))} ||
		 IncludeDir <- IncludeDir2] ++
	     [{Node2,get_absdir(filename:join(SpecDir,IncludeDir))} ||
		 IncludeDir <- IncludeDir2]],
    
    Verify = #testspec{spec_dir = SpecDir,
		       nodes = [{undefined,Node},{n1,Node1},{n2,Node2}],
		       init = [{Node1,[{node_start,[{callback_module,cbm}]},
				       {eval,[{mod,func,[]}]}]},
			       {Node2,[{node_start,[{callback_module,ct_slave}]},
				       {eval,[{mod,func,[]}]}]},
			       {Node,[{node_start,[]},
				      {eval,[{mod,func,[]}]}]}],
		       label = [{Node,"basic_compatible_nodes"},
				{Node2,"basic_compatible_nodes"},
				{Node1,basic_compatible_nodes_1}],
		       logdir = [{Node,LogDirV},{Node1,LogDirV},{Node2,LogDirV},
				 {master,MasterLogDirV},"."],
		       logopts = [{Node2,[no_nl]}],
		       basic_html = [],
		       cover = [],
		       config = CFGs,
		       userconfig = [{Node,{?MODULE,"cfg_str1"}},
				     {Node1,{?MODULE,"cfg_str1"}},
				     {Node2,{?MODULE,"cfg_str1"}},
				     {Node,{?MODULE,"cfg_str2"}},
				     {Node1,{?MODULE,"cfg_str2"}},
				     {Node2,{?MODULE,"cfg_str2"}}],
		       event_handler = [{master,evh1,[1]},
					{Node1,evh2,[[2,3]]},
					{Node1,evh3,[[2,3]]},
					{Node2,evh2,[[2,3]]},
					{Node2,evh3,[[2,3]]}],
		       ct_hooks = [{Node,{cth_mod1,[]}},
				   {Node1,{cth_mod1,[]}},
				   {Node2,{cth_mod1,[]}},
				   {Node,{cth_mod2,[]}},
				   {Node1,{cth_mod2,[]}},
				   {Node2,{cth_mod2,[]}}],
		       enable_builtin_hooks = true,
		       release_shell = false,
		       include = Incls,
		       auto_compile = [],
		       stylesheet = [],
		       multiply_timetraps = [{Node1,2}],
		       scale_timetraps = [],
		       create_priv_dir = [],
		       tests = [{{Node1,TO1V},
				 [{x_SUITE,[all]},
				  {y_SUITE,[{g1,all},{g2,all},tc1,tc2]},
				  {z_SUITE,[{all,{skip,"skipped"}}]}]},
				{{Node2,TO2V},
				 [{x_SUITE,[all,
					    {{g1,all},{skip,"skipped"}},
					    {{g2,all},{skip,"skipped"}}]},
				  {y_SUITE,[all,
					    {tc1,{skip,"skipped"}},
					    {tc2,{skip,"skipped"}}]}]}],
		       merge_tests = true},

    verify_result(Verify,ListResult,FileResult),

    {ok,Tests} = ct_testspec:get_tests([SpecFile]),
    ct:pal("ct_testspec:get_tests/1:~n~p~n", [Tests]),
    [{[SpecFile],[{Node,[],[]},
                  {Node1,Run1,Skip1},
                  {Node2,Run2,Skip2}]}] = Tests,
    [{TO1V,x_SUITE,all},
     {TO1V,y_SUITE,[{g1,all},{g2,all},tc1,tc2]},
     {TO1V,z_SUITE,all}] = lists:sort(Run1),
    [{TO2V,x_SUITE,all},
     {TO2V,y_SUITE,all}] = lists:sort(Run2),
    [{TO1V,z_SUITE,"skipped"}] = lists:sort(Skip1),
    [{TO2V,x_SUITE,{g1,all},"skipped"},
     {TO2V,x_SUITE,{g2,all},"skipped"},
     {TO2V,y_SUITE,tc1,"skipped"},
     {TO2V,y_SUITE,tc2,"skipped"}] = lists:sort(Skip2),

    ok.

%%%-----------------------------------------------------------------
%%%
unknown_terms(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Spec1 = [{suites,PrivDir,all},
	     {userdata,"I've got news for you"}],
    {error,{undefined_term_in_spec,{userdata,_}}} =
	(catch ct_testspec:collect_tests_from_list(Spec1, false)),
    true = is_record(ct_testspec:collect_tests_from_list(Spec1, true),
		      testspec),
    
    Spec2 = [{logdir,{logdir,PrivDir}}],
    {error,{invalid_directory_name,_}} =
	(catch ct_testspec:collect_tests_from_list(Spec2, false)),
    
    Spec3 = [{suite,PrivDir,all}],
    {error,{undefined_term_in_spec,{suite,_,_}}} =
	(catch ct_testspec:collect_tests_from_list(Spec3, false)),
    true = is_record(ct_testspec:collect_tests_from_list(Spec3, true), testspec),
    
    Spec4 = [{suites,PrivDir,all},
	     {skip_suites,PrivDir,x_SUITE}],
    {error,{bad_term_in_spec,{skip_suites,_,_}}} =
	(catch ct_testspec:collect_tests_from_list(Spec4, false)),
    {error,{bad_term_in_spec,{skip_suites,_,_}}} =
	(catch ct_testspec:collect_tests_from_list(Spec4, true)),

    Spec5 = [{configs,all_nodes,PrivDir}],
    {error,{undefined_term_in_spec,{configs,_,_}}} =
	(catch ct_testspec:collect_tests_from_list(Spec5, false)),
    true = is_record(ct_testspec:collect_tests_from_list(Spec5, true), testspec),
  
    ok.

%%%-----------------------------------------------------------------
%%%
no_merging(_Config) ->    
    Node1 = node1@host1,
    Node2 = node2@host2,
    TODir1 = "../tests/to1",
    TODir2 = "../tests/to2",
    Spec =
	[
	 {merge_tests,false},
	 {node,n1,Node1},
	 {node,n2,Node2},
	 {suites,n1,TODir1,[x_SUITE]},
	 {groups,n1,TODir1,y_SUITE,[g1,g2]},
	 {cases,n1,TODir1,y_SUITE,[tc1,tc2]},
	 {skip_suites,n1,TODir1,z_SUITE,"skipped"},
	 {suites,n2,TODir2,[x_SUITE,y_SUITE]},
	 {skip_groups,n2,TODir2,x_SUITE,[g1,g2],"skipped"},
	 {skip_cases,n2,TODir2,y_SUITE,[tc1,tc2],"skipped"}
	],
    
    {ok,SpecDir} = file:get_cwd(),
    
    ListResult = ct_testspec:collect_tests_from_list(Spec, false),
    ct:pal("TESTSPEC RECORD FROM LIST:~n~p~n", [rec2proplist(ListResult)]),	
    SpecFile = ct_test_support:write_testspec(Spec,SpecDir,
					      "no_merging.spec"),
    FileResult = ct_testspec:collect_tests_from_file([SpecFile], false),
    ct:pal("TESTSPEC RECORD FROM FILE:~n~p~n", [rec2proplist(FileResult)]),
    
    Node = node(),
    TO1V = get_absdir(filename:join(SpecDir,TODir1)),
    TO2V = get_absdir(filename:join(SpecDir,TODir2)),
    
    Verify = #testspec{merge_tests = false,
		       spec_dir = SpecDir,
		       nodes = [{undefined,Node},{n1,Node1},{n2,Node2}],
		       tests = [{{Node1,TO1V},
				 [{x_SUITE,[all]}]},
				{{Node1,TO1V},
				 [{y_SUITE,[{g1,all},{g2,all}]}]},
				{{Node1,TO1V},
				 [{y_SUITE,[tc1,tc2]}]},
				{{Node1,TO1V},
				 [{z_SUITE,[{all,{skip,"skipped"}}]}]},
				{{Node2,TO2V},
				 [{x_SUITE,[all]}]},
				{{Node2,TO2V},
				 [{y_SUITE,[all]}]},
				{{Node2,TO2V},
				 [{x_SUITE,[{{g1,all},{skip,"skipped"}},
					    {{g2,all},{skip,"skipped"}}]}]},
				{{Node2,TO2V},
				 [{y_SUITE,[{tc1,{skip,"skipped"}},
					    {tc2,{skip,"skipped"}}]}]}]},
		       
    verify_result(Verify,ListResult,FileResult),

    {ok,Tests} = ct_testspec:get_tests([SpecFile]),
    ct:pal("ct_testspec:get_tests/1:~n~p~n", [Tests]),
    [{[SpecFile],[{Node,[],[]},
                  {Node1,Run1,Skip1},
                  {Node2,Run2,Skip2}]}] = Tests,
    [{TO1V,x_SUITE,all},
     {TO1V,y_SUITE,[tc1,tc2]},
     {TO1V,y_SUITE,[{g1,all},{g2,all}]},
     {TO1V,z_SUITE,all}] = lists:sort(Run1),
    [{TO2V,x_SUITE,all},
     {TO2V,x_SUITE,[{skipped,g1,all},{skipped,g2,all}]},
     {TO2V,y_SUITE,all},
     {TO2V,y_SUITE,[{skipped,tc1},{skipped,tc2}]}] = lists:sort(Run2),
    [{TO1V,z_SUITE,"skipped"}] = lists:sort(Skip1),
    [{TO2V,x_SUITE,{g1,all},"skipped"},
     {TO2V,x_SUITE,{g2,all},"skipped"},
     {TO2V,y_SUITE,tc1,"skipped"},
     {TO2V,y_SUITE,tc2,"skipped"}] = lists:sort(Skip2),

    ok.

%%%-----------------------------------------------------------------
%%%
multiple_specs(_Config) ->
    Node1 = node1@host1,
    Node2 = node2@host2,
    TODir1 = "../tests/to1",
    TODir2 = "../tests/to2",
    CfgDir1 = "../cfgs/to1/x.cfg",
    CfgDir2 = ["../cfgs/to2/x.cfg","../cfgs/to2/y.cfg"],
    LogDir = "../logs",
    Spec1 =
	[
	 {node,n1,Node1},
	 {node,n2,Node2},
	 {alias,to1,TODir1},
	 {alias,to2,TODir2},
	 {label,"multiple_specs1"},
	 {config,n1,CfgDir1},
	 {config,n2,CfgDir2},
	 {logdir,all_nodes,LogDir}
	],
    Spec2 =
	[
	 {merge_tests,false},
	 {label,"multiple_specs2"},
	 {suites,n1,TODir1,[x_SUITE]},
	 {groups,n1,TODir1,y_SUITE,[g1,g2]},
	 {cases,n1,TODir1,y_SUITE,[tc1,tc2]},
	 {skip_suites,n1,TODir1,z_SUITE,"skipped"},
	 {suites,n2,TODir2,[x_SUITE,y_SUITE]},
	 {skip_groups,n2,TODir2,x_SUITE,[g1,g2],"skipped"},
	 {skip_cases,n2,TODir2,y_SUITE,[tc1,tc2],"skipped"}
	],
    
    {ok,SpecDir} = file:get_cwd(),    
    SpecFile1 = ct_test_support:write_testspec(Spec1,SpecDir,
					      "multiple_specs.1.spec"),
    SpecFile2 = ct_test_support:write_testspec(Spec2,SpecDir,
					      "multiple_specs.2.spec"),
    FileResult = ct_testspec:collect_tests_from_file([[SpecFile1,SpecFile2]],
						     false),
    ct:pal("TESTSPEC RECORD FROM FILE:~n~p~n", [rec2proplist(FileResult)]),
    
    Node = node(),
    TO1V = get_absdir(filename:join(SpecDir,TODir1)),
    TO2V = get_absdir(filename:join(SpecDir,TODir2)),
    CFGs = [{Node1,get_absdir(filename:join(SpecDir,CfgDir1))} |
	    [{Node2,get_absdir(filename:join(SpecDir,CfgDir))} || CfgDir <- CfgDir2]],
    LogDirV = get_absdir(filename:join(SpecDir,"../logs")),

    Verify = #testspec{merge_tests = true,
		       spec_dir = SpecDir,
		       nodes = [{undefined,Node},{n1,Node1},{n2,Node2}],
		       alias = [{to1,TO1V},{to2,TO2V}],
		       label = [{Node,"multiple_specs1"},
				{Node1,"multiple_specs1"},
				{Node2,"multiple_specs1"}],
		       logdir = [{Node,LogDirV},{Node1,LogDirV},{Node2,LogDirV},"."],
		       config = CFGs,
		       tests = [{{Node1,TO1V},
				 [{x_SUITE,[all]},
				  {y_SUITE,[{g1,all},{g2,all},tc1,tc2]},
				  {z_SUITE,[{all,{skip,"skipped"}}]}]},
				{{Node2,TO2V},
				 [{x_SUITE,[all,{{g1,all},{skip,"skipped"}},
					    {{g2,all},{skip,"skipped"}}]},
				  {y_SUITE,[all,{tc1,{skip,"skipped"}},
					    {tc2,{skip,"skipped"}}]}]}]},
		       
    verify_result(Verify,FileResult,FileResult),

    {ok,Tests} = ct_testspec:get_tests([[SpecFile1,SpecFile2]]),
    ct:pal("ct_testspec:get_tests/1:~n~p~n", [Tests]),
    [{[SpecFile1,SpecFile2],[{Node,[],[]},
                             {Node1,Run1,Skip1},
                             {Node2,Run2,Skip2}]}] = Tests,
    [{TO1V,x_SUITE,all},
     {TO1V,y_SUITE,[{g1,all},{g2,all},tc1,tc2]},
     {TO1V,z_SUITE,all}] = lists:sort(Run1),
    [{TO2V,x_SUITE,all},
     {TO2V,y_SUITE,all}] = lists:sort(Run2),
    [{TO1V,z_SUITE,"skipped"}] = lists:sort(Skip1),
    [{TO2V,x_SUITE,{g1,all},"skipped"},
     {TO2V,x_SUITE,{g2,all},"skipped"},
     {TO2V,y_SUITE,tc1,"skipped"},
     {TO2V,y_SUITE,tc2,"skipped"}] = lists:sort(Skip2),

    ok.

%%%-----------------------------------------------------------------
%%% 
misc_config_terms(_Config) ->
    CfgDir = "../cfgs/to1",
    TODir = "../tests/to1",
    Spec =
	[{node,x,n1@h1},{node,y,n2@h2},

	 {config,CfgDir,"a.cfg"},
	 {config,n1@h1,CfgDir,"b.cfg"},
	 {config,all_nodes,CfgDir,"c.cfg"},
	 {config,all_nodes,filename:join(CfgDir,"d.cfg")},

	 {basic_html,true},
	 {basic_html,n1@h1,false},
	 {basic_html,n2@h2,true},
	 
	 {silent_connections,n1@h1,all},
	 {silent_connections,n2@h2,[ssh]},

	 {enable_builtin_hooks,false},

	 {release_shell,true},

	 {auto_compile,false},
	 {auto_compile,n1@h1,true},
	 {auto_compile,n2@h2,false},

	 {stylesheet,"../css"},
	 {stylesheet,n1@h1,"./n1/css"},
	 {stylesheet,n2@h2,"./n2/css"},

	 {create_priv_dir,[auto_per_tc]},
	 {create_priv_dir,n1@h1,[manual_per_tc]},
	 {create_priv_dir,n2@h2,[auto_per_run]},

	 {suites,n1@h1,TODir,[x_SUITE]}
	],
    
    {ok,SpecDir} = file:get_cwd(),
    
    ListResult = ct_testspec:collect_tests_from_list(Spec, false),
    ct:pal("TESTSPEC RECORD FROM LIST:~n~p~n", [rec2proplist(ListResult)]),	
    SpecFile = ct_test_support:write_testspec(Spec,SpecDir,
					      "misc_config_terms.spec"),
    FileResult = ct_testspec:collect_tests_from_file([SpecFile], false),
    ct:pal("TESTSPEC RECORD FROM FILE:~n~p~n", [rec2proplist(FileResult)]),
    
    Node = node(),
    CfgA = get_absdir(filename:join(filename:join(SpecDir,CfgDir), "a.cfg")),
    CfgB = get_absdir(filename:join(filename:join(SpecDir,CfgDir), "b.cfg")),
    CfgC = get_absdir(filename:join(filename:join(SpecDir,CfgDir), "c.cfg")),
    CfgD = get_absdir(filename:join(filename:join(SpecDir,CfgDir), "d.cfg")),
    CSS = get_absdir(filename:join(SpecDir,"../css")),
    CSS1 = get_absdir(filename:join(SpecDir,"./n1/css")),
    CSS2 = get_absdir(filename:join(SpecDir,"./n2/css")),
    
    Verify = #testspec{spec_dir = SpecDir,
		       nodes = [{undefined,Node},{x,n1@h1},{y,n2@h2}],
		       basic_html = [{Node,true},{n1@h1,false},{n2@h2,true}],
		       silent_connections = [{n1@h1,[all]},{n2@h2,[ssh]}],
		       config = [{Node,CfgA},
				 {n1@h1,CfgA},
				 {n2@h2,CfgA},
				 {n1@h1,CfgB},
				 {Node,CfgC},
				 {n1@h1,CfgC},
				 {n2@h2,CfgC},
				 {Node,CfgD},
				 {n1@h1,CfgD},
				 {n2@h2,CfgD}],
		       enable_builtin_hooks = false,
		       release_shell = true,
		       auto_compile = [{Node,false},
				       {n1@h1,true},
				       {n2@h2,false}],
		       stylesheet = [{Node,CSS},
				     {n1@h1,CSS1},
				     {n2@h2,CSS2}],	       
		       create_priv_dir = [{Node,[auto_per_tc]},
					  {n1@h1,[manual_per_tc]},
					  {n2@h2,[auto_per_run]}],
		       tests = [{{n1@h1,get_absdir(filename:join(SpecDir,TODir))},
				 [{x_SUITE,[all]}]}]
		      },
    
    verify_result(Verify,ListResult,FileResult).

%%%-----------------------------------------------------------------
%%% 
define_names_1(_Config) ->
    Spec =
	[
	 {define,'HOST','eniac'},
	 {define,'NODE1',testnode1},
	 {define,'NODE2',testnode2},
	 {define,'NODES',['NODE1@HOST',
			  'NODE2@HOST']},
	 {define,'TOPDIR',".."},
	 {define,'TO1',"to1"},
	 {define,'TO2',"to2"},
	 {define,'LOGDIR',"'TOPDIR'/logdir"},
	 {define,'LOGDIR1',"'TOPDIR'/logdir1"},
	 {define,'LOGDIR2',"'TOPDIR'/logdir2"},
	 {define,'CFGDIR',"'TOPDIR'/cfgs"},
	 {define,'CFGFILES',["cfgX","cfgY"]},
	 {define,'TESTDIR',"'TOPDIR'/test"},
	 {define,'TO1DIR',"'TESTDIR'/'TO1'"},
	 {define,'TO2DIR',"'TESTDIR'/'TO2'"},
	 {define,'EXSUITE',ex_SUITE},
	 {define,'EXGRS',[g1,g2]},

	 {logdir,'LOGDIR'},
	 {logdir,'NODE1@HOST','LOGDIR1'},
	 {logdir,'NODE2@HOST','LOGDIR2'},

	 {config,["a.cfg","b.cfg"]},
	 {config,'NODES',"./'CFGDIR'/c.cfg"},
	 {config,'CFGDIR',["d.cfg","e.cfg"]},
	 {config,'NODE2@HOST','CFGDIR','CFGFILES'},

	 {suites,'NODE1@HOST','TO1DIR',all},
	 {suites,'NODES','TO2DIR',all},

	 {groups,'TO1DIR','EXSUITE','EXGRS'}
	],
    
    {ok,SpecDir} = file:get_cwd(),
    
    ListResult = ct_testspec:collect_tests_from_list(Spec, false),
    ct:pal("TESTSPEC RECORD FROM LIST:~n~p~n", [rec2proplist(ListResult)]),	
    SpecFile = ct_test_support:write_testspec(Spec,SpecDir,
					      "define_names_1.spec"),
    FileResult = ct_testspec:collect_tests_from_file([SpecFile], false),
    ct:pal("TESTSPEC RECORD FROM FILE:~n~p~n", [rec2proplist(FileResult)]),
    
    N = node(),
    N1 = testnode1@eniac,
    N2 = testnode2@eniac,
    Join = fun(Dir) -> shorten_path(filename:join(SpecDir,Dir),SpecDir) end,

    Verify = #testspec{spec_dir = SpecDir,
		       nodes = [{undefined,N2},
				{undefined,N1},
				{undefined,N}],
		       config = [{N2,Join("a.cfg")},{N2,Join("b.cfg")},
				 {N1,Join("a.cfg")},{N1,Join("b.cfg")},
				 {N,Join("a.cfg")},{N,Join("b.cfg")},
				 {N1,Join("../cfgs/c.cfg")},{N2,Join("../cfgs/c.cfg")},
				 {N2,Join("../cfgs/d.cfg")},{N2,Join("../cfgs/e.cfg")},
				 {N1,Join("../cfgs/d.cfg")},{N1,Join("../cfgs/e.cfg")},
				 {N,Join("../cfgs/d.cfg")},{N,Join("../cfgs/e.cfg")},
				 {N2,Join("../cfgs/cfgX")},{N2,Join("../cfgs/cfgY")}],
		       logdir = [{N,Join("../logdir")},
				 {N1,Join("../logdir1")},
				 {N2,Join("../logdir2")},
				 "."],
		       tests = [{{N1,Join("../test/to1")},[{all,[all]}]},
				{{N1,Join("../test/to2")},[{all,[all]}]},
				{{N2,Join("../test/to2")},[{all,[all]}]},
				{{N2,Join("../test/to1")},
				 [{ex_SUITE,[{g1,all},{g2,all}]}]},
				{{N,Join("../test/to1")},
				 [{ex_SUITE,[{g1,all},{g2,all}]}]}]
		      },    
    verify_result(Verify,ListResult,FileResult).


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

verify_result(VerificationRec,ListResult,FileResult) ->
    {_,TSLTuples} = rec2proplist(ListResult),
    {_,TSFTuples} = rec2proplist(FileResult),
    {_,VTuples} = rec2proplist(VerificationRec),    
    VResult =
	(catch lists:foldl(fun({Tag,Val},{[{Tag,Val}|TSL],[{Tag,Val}|TSF]}) ->
				   {TSL,TSF};
			      ({Tag,Val},{[{_Tag,TSLVal}|_TSL],[{Tag,Val}|_TSF]}) ->
				   exit({ts_list_mismatch,Tag,Val,TSLVal});
			      ({Tag,Val},{[{Tag,Val}|_TSL],[{_Tag,TSFVal}|_TSF]}) ->
				   exit({ts_file_mismatch,Tag,Val,TSFVal});
			      ({Tag,Val},{_,[{_Tag,TSFVal}|_TSF]}) ->
				   exit({ts_mismatch,Tag,Val,TSFVal})
			   end, {TSLTuples,TSFTuples}, VTuples)),
	       case VResult of
		  {'EXIT',Reason} ->
		      ct:fail(Reason);
		  _ ->
		      ok
	       end,
    ok.


check_parameter(S="cfg_str1") ->
    {ok,{config,S}};
check_parameter(S="cfg_str2") ->
    {ok,{config,S}}.
read_config(S) ->
    {ok,[{cfg,S}]}.

rec2proplist(E={error,_What}) ->
    exit({invalid_testspec_record,E});
rec2proplist([{Specs,Rec}]) when is_list(Specs) ->
    rec2proplist(Rec);
rec2proplist(Rec) ->
    [RecName|RecList] = tuple_to_list(Rec),
    FieldNames = 
	if RecName == testspec ->
		record_info(fields, testspec);
	   true ->
		undefined
	end,
    {RecName,combine_names_and_vals(FieldNames,RecList)}.

combine_names_and_vals([FN|FNs], [V|Vs]) ->
    [{FN,V} | combine_names_and_vals(FNs, Vs)];
combine_names_and_vals([], []) ->
    [];
combine_names_and_vals(_, _) ->
    [].

get_absdir(Dir) ->
    shorten_path(filename:absname(Dir),Dir).

shorten_path(Path,SpecDir) ->
    case shorten_split_path(filename:split(Path),[]) of
	[] ->
	    [Root|_] = filename:split(SpecDir),
	    Root;
	Short ->	    
	    filename:join(Short)
    end.

shorten_split_path([".."|Path],SoFar) ->
    shorten_split_path(Path,tl(SoFar));
shorten_split_path(["."|Path],SoFar) ->
    shorten_split_path(Path,SoFar);
shorten_split_path([Dir|Path],SoFar) ->
    shorten_split_path(Path,[Dir|SoFar]);
shorten_split_path([],SoFar) ->
    lists:reverse(SoFar).
