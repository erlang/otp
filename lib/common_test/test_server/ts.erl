%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
%%% File    : ts.erl
%%% Purpose : Frontend for running tests.
%%%-------------------------------------------------------------------

-module(ts).

-export([cl_run/1,
	 run/0, run/1, run/2, run/3, run/4, run/5,
	 run_category/1, run_category/2, run_category/3,
	 tests/0, tests/1, suites/1, categories/1,
	 install/0, install/1,
	 estone/0, estone/1,
	 cross_cover_analyse/1,
	 compile_testcases/0, compile_testcases/1,
	 help/0]).

%% Functions kept for backwards compatibility
-export([bench/0, bench/1, bench/2, benchmarks/0,
	 smoke_test/0, smoke_test/1,smoke_test/2, smoke_tests/0]).

-export([i/0, l/1, r/0, r/1, r/2, r/3]).

%%%----------------------------------------------------------------------
%%% This module, ts, is the interface to all of the functionality of
%%% the TS framework.  The picture below shows the relationship of
%%% the modules:
%%%
%%%       +-- ts_install --+------  ts_autoconf_win32
%%%       |
%%% ts ---+                +------  ts_erl_config
%%%       |                |				     ts_lib
%%%       +-- ts_run  -----+------  ts_make
%%%       |                |	    			     ts_filelib
%%%       |                +------  ts_make_erl
%%%       |
%%%       +-- ts_benchmark
%%%
%%% The modules ts_lib and ts_filelib contains utilities used by
%%% the other modules.
%%%
%%% Module		 Description
%%% ------		 -----------
%%% ts			 Frontend to the test server framework.  Contains all
%%%			 interface functions.
%%% ts_install		 Installs the test suite.  On Unix, `autoconf' is
%%%			 is used; on Windows, ts_autoconf_win32 is used.
%%%			 The result is written to the file `variables'.
%%% ts_run		 Supervises running of the tests.
%%% ts_autconf_win32	 An `autoconf' for Windows.
%%% ts_autconf_cross_env `autoconf' for other platforms (cross environment)
%%% ts_erl_config	 Finds out information about the Erlang system,
%%%			 for instance the location of erl_interface.
%%%			 This works for either an installed OTP or an Erlang
%%%			 system running in a git repository/source tree.
%%% ts_make		 Interface to run the `make' program on Unix
%%%			 and other platforms.
%%% ts_make_erl		 A corrected version of the standar Erlang module
%%%			 make (used for rebuilding test suites).
%%% ts_lib		 Miscellanous utility functions, each used by several
%%%			 other modules.
%%% ts_benchmark         Supervises otp benchmarks and collects results.
%%%----------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").
-include("ts.hrl").

-define(
   install_help,
   [
    "  ts:install()\n",
    "    Install ts with no options.\n",
    "\n",
    "  ts:install(Options)\n",
    "    Install ts with a list of options, see below.\n",
    "\n",
    "Installation options supported:\n\n",
    "  {longnames, true} - Use fully qualified hostnames\n",
    "  {verbose, Level}  - Sets verbosity level for TS output (0,1,2), 0 is\n"
    "                      quiet(default).\n"
    "  {crossroot, ErlTop}\n"
    "                    - Erlang root directory on build host, ~n"
    "                      normally same value as $ERL_TOP\n"
    "  {crossenv, [{Key,Val}]}\n"
    "                    - Environmentals used by test configure on build host\n"
    "  {crossflags, FlagsString}\n"
    "                    - Flags used by test configure on build host\n"
    "  {xcomp, XCompFile}\n"
    "                    - The xcomp file to use for cross compiling the~n"
    "                      testcases. Using this option will override any~n"
    "                      cross* configurations given to ts. Note that you~n"
    "                      have to have a correct ERL_TOP as well.~n"
   ]).

help() ->
    case filelib:is_file(?variables) of
	false -> help(uninstalled);
	true  -> help(installed)
    end.

help(uninstalled) ->
    H = ["ts is not yet installed. To install use:\n\n"],
    show_help([H,?install_help]);
help(installed) ->
    H = ["\n",
	 "Run functions:\n\n",
	 "  ts:run()\n",
	 "    Run the tests for all apps. The tests are defined by the\n",
	 "    main test specification for each app: ../App_test/App.spec.\n",
	 "\n",
	 "  ts:run(Apps)\n",
	 "    Apps = atom() | [atom()]\n",
	 "    Run the tests for an app, or set of apps. The tests are\n",
	 "    defined by the main test specification for each app:\n",
	 "    ../App_test/App.spec.\n",
	 "\n",
	 "  ts:run(App, Suites)\n",
	 "    App = atom(), Suites = atom() | [atom()]\n",
	 "    Run one or more test suites for App (i.e. modules named\n",
	 "    *_SUITE.erl, located in ../App_test/).\n",
	 "\n",
	 "  ts:run(App, Suite, TestCases)\n",
	 "    App = atom(), Suite = atom(),\n",
	 "    TestCases = TCs | {testcase,TCs}, TCs = atom() | [atom()]\n",
	 "    Run one or more test cases (functions) in Suite.\n",
	 "\n",
	 "  ts:run(App, Suite, {group,Groups})\n",
	 "    App = atom(), Suite = atom(), Groups = atom() | [atom()]\n",
	 "    Run one or more test case groups in Suite.\n",
	 "\n",
	 "  ts:run(App, Suite, {group,Group}, {testcase,TestCases})\n",
	 "    App = atom(), Suite = atom(), Group = atom(),\n",
	 "    TestCases = atom() | [atom()]\n",
 	 "    Run one or more test cases in a test case group in Suite.\n",
	 "\n",
	 "  ts:run_category(TestCategory)\n",
	 "    TestCategory = smoke | essential | bench | atom()\n",
	 "    Run the specified category of tests for all apps.\n",
	 "    For each app, the tests are defined by the specification:\n",
	 "    ../App_test/App_TestCategory.spec.\n",
	 "\n",
	 "  ts:run_category(Apps, TestCategory)\n",
	 "    Apps = atom() | [atom()],\n",
	 "    TestCategory = smoke | essential | bench | atom()\n",
	 "    Run the specified category of tests for the given app or apps.\n",
	 "\n",
	 "    Note that the test category parameter may have arbitrary value,\n",
	 "    but should correspond to an existing test specification with file\n",
	 "    name: ../App_test/App_TestCategory.spec.\n",
	 "    Predefined categories exist for smoke tests, essential tests and\n",
	 "    benchmark tests. The corresponding specs are:\n",
	 "    ../*_test/Spec_smoke.spec, ../*_test/Spec_essential.spec and\n",
	 "    ../*_test/Spec_bench.spec.\n",
	 "\n",
	 "  All above run functions can take an additional last argument,\n",
	 "  Options, which is a list of options (e.g. ts:run(App, Options),\n",
	 "  or ts:run_category(Apps, TestCategory, Options)).\n",
	 "\n",
	 "Run options supported:\n\n",
	 "  batch             - Do not start a new xterm\n",
	 "  {verbose, Level}  - Same as the verbosity option for install\n",
	 "  verbose           - Same as {verbose, 1}\n",
	 "  {vars, Vars}      - Variables in addition to the 'variables' file\n",
	 "                      Can be any of the install options\n",
	 "  {trace, TraceSpec}- Start call trace on target and slave nodes\n",
	 "                      TraceSpec is the name of a file containing\n",
	 "                      trace specifications or a list of trace\n",
	 "                      specification elements.\n",
	 "  {config, Path}    - Specify which directory ts should get it's \n"
	 "                      config files from. The files should follow\n"
	 "                      the convention lib/test_server/src/ts*.config.\n"
	 "                      These config files can also be specified by\n"
	 "                      setting the TEST_CONFIG_PATH environment\n"
	 "                      variable to the directory where the config\n"
	 "                      files are. The default location is\n"
	 "                      tests/test_server/.\n"
	 "\n",
	 "Supported trace information elements:\n\n",
	 "  {tp | tpl, Mod, [] | match_spec()}\n",
	 "  {tp | tpl, Mod, Func, [] | match_spec()}\n",
	 "  {tp | tpl, Mod, Func, Arity, [] | match_spec()}\n",
	 "  {ctp | ctpl, Mod}\n",
	 "  {ctp | ctpl, Mod, Func}\n",
	 "  {ctp | ctpl, Mod, Func, Arity}\n",
	 "\n\n",
	 "Support functions:\n\n",
	 "  ts:tests()\n",
	 "    Returns all apps available for testing.\n",
	 "\n",
	 "  ts:tests(TestCategory)\n",
	 "    Returns all apps that provide tests in the given category.\n",
	 "\n",
	 "  ts:suites(App)\n",
	 "    Returns all available test suites for App,\n",
	 "    i.e. ../App_test/*_SUITE.erl\n",
	 "\n",
	 "  ts:categories(App)\n",
	 "    Returns all test categories available for App.\n",
	 "\n",
	 "  ts:estone()\n",
	 "    Runs estone_SUITE in the kernel application with no run options\n",
	 "\n",
	 "  ts:estone(Opts)\n",
	 "    Runs estone_SUITE in the kernel application with the given\n",
	 "    run options\n",
	 "\n",
	 "  ts:cross_cover_analyse(Level)\n",
	 "    Use after ts:run with option cover or cover_details. Analyses\n",
	 "    modules specified with a 'cross' statement in the cover spec file.\n",
	 "    Level can be 'overview' or 'details'.\n",
	 "\n",
	 "  ts:compile_testcases()\n",
	 "  ts:compile_testcases(Apps)\n",
	 "    Compiles all test cases for the given apps, for usage in a\n",
	 "    cross compilation environment.\n",
	 "\n\n",
	 "Installation (already done):\n\n"
	],
    show_help([H,?install_help]).

show_help(H) ->
    io:format(lists:flatten(H)).


%% Installs tests.
install() ->
    ts_install:install(install_local,[]).
install(Options) when is_list(Options) ->
    ts_install:install(install_local,Options).

%% run/0
%%  Runs all specs found by ts:tests(), if any, or returns
%%  {error, no_tests_available}. (batch)
run() ->
    case ts:tests() of
	[] ->
	    {error, no_tests_available};
	_ ->
	    check_and_run(fun(Vars) -> run_all(Vars) end)
    end.
run_all(_Vars) ->
    run_some(tests(), [batch]).

run_some([], _Opts) ->
    ok;
run_some(Apps, Opts) ->
    case proplists:get_value(test_category, Opts) of
	bench ->
	    check_and_run(fun(Vars) -> ts_benchmark:run(Apps, Opts, Vars) end);
	_Other ->
	    run_some1(Apps, Opts)
    end.

run_some1([], _Opts) ->
    ok;
run_some1([{App,Mod}|Apps], Opts) ->
    case run(App, Mod, Opts) of
	ok -> ok;
	Error -> io:format("~p: ~p~n",[{App,Mod},Error])
    end,
    run_some1(Apps, Opts);
run_some1([App|Apps], Opts) ->
    case run(App, Opts) of
	ok -> ok;
	Error -> io:format("~p: ~p~n",[App,Error])
    end,
    run_some1(Apps, Opts).

%% This can be used from command line. Both App and
%% TestCategory must be specified. App may be 'all'
%% and TestCategory may be 'main'. Examples:
%% erl -s ts cl_run kernel smoke <options>
%% erl -s ts cl_run kernel main <options>
%% erl -s ts cl_run all essential <options>
%% erl -s ts cl_run all main <options>
%% When using the 'main' category and running with cover,
%% one can also use the cross_cover_analysis flag.
cl_run([App,Cat|Options0]) when is_atom(App) ->

    AllAtomsFun = fun(X) when is_atom(X) -> true; 
		     (_) -> false 
		  end,
    Options1 = 
	case lists:all(AllAtomsFun, Options0) of
	    true ->
		%% Could be from command line
		lists:map(fun(Opt) ->
				  to_erlang_term(Opt)
			  end, Options0) -- [batch];
	    false ->
		Options0 -- [batch]
	end,
    %% Make sure there is exactly one occurence of 'batch'
    Options2 = [batch|Options1],

    Result =
	case {App,Cat} of
	    {all,main} ->
		run(tests(), Options2);
	    {all,Cat} ->
		run_category(Cat, Options2);
	    {_,main} ->
		run(App, Options2);
	    {_,Cat} ->
		run_category(App, Cat, Options2)
	end,
    case check_for_cross_cover_analysis_flag(Options2) of
	false ->
	    ok;
	Level ->
	    cross_cover_analyse(Level)
    end,
    Result.

%% run/1
%% Runs tests for one app (interactive).
run(App) when is_atom(App) ->
    Options = check_test_get_opts(App, []),
    File = atom_to_list(App),
    run_test(File, [{spec,[File++".spec"]},{allow_user_terms,true}], Options);

%% This can be used from command line, e.g.
%% erl -s ts run all <options>
%% erl -s ts run main <options>
run([all,main|Opts]) ->
    cl_run([all,main|Opts]);
run([all|Opts]) ->
    cl_run([all,main|Opts]);
run([main|Opts]) ->
    cl_run([all,main|Opts]);
%% Backwards compatible
run([all_tests|Opts]) ->
    cl_run([all,main|Opts]);

%% run/1
%% Runs the main tests for all available apps
run(Apps) when is_list(Apps) ->
    run(Apps, [batch]).

%% run/2
%% Runs the main tests for all available apps
run(Apps, Opts) when is_list(Apps), is_list(Opts) ->
    run_some(Apps, Opts);

%% Runs tests for one app with list of suites or with options
run(App, ModsOrOpts) when is_atom(App),
			  is_list(ModsOrOpts) ->
    case is_list_of_suites(ModsOrOpts) of
	false ->
	    run(App, {opts_list,ModsOrOpts});
	true ->
	    run_some([{App,M} || M <- ModsOrOpts],
		     [batch])
    end;

run(App, {opts_list,Opts}) ->
    Options = check_test_get_opts(App, Opts),
    File = atom_to_list(App),

    %% check if other test category than main has been specified
    {CatSpecName,TestCat} =
	case proplists:get_value(test_category, Opts) of
	    undefined ->
		{"",main};
	    Cat ->
		{"_" ++ atom_to_list(Cat),Cat}
	end,

    WhatToDo =
	case App of
	    %% Known to exist but fails generic tests below
	    emulator -> test;
	    system -> test;
	    erl_interface -> test;
	    epmd -> test;
	    _ ->
		case code:lib_dir(App) of
		    {error,bad_name} ->
			%% Application does not exist
			skip;
		    Path ->
			case file:read_file_info(filename:join(Path,"ebin")) of
			    {ok,#file_info{type=directory}} ->
				%% Erlang application is built
				test;
			    _ ->
				case filelib:wildcard(
				       filename:join([Path,"priv","*.jar"])) of
				    [] ->
					%% The application is not built
					skip;
				    [_|_] ->
					%% Java application is built
					test
				end
			end
		end
	end,
    case WhatToDo of
	skip ->
	    SkipSpec = create_skip_spec(App, suites(App)),
	    run_test(File, [{spec,[SkipSpec]}], Options);
	test when TestCat == bench ->
	    check_and_run(fun(Vars) ->
				  ts_benchmark:run([App], Options, Vars)
			  end);
	test ->
	    Spec = File ++ CatSpecName ++ ".spec",
	    run_test(File, [{spec,[Spec]},{allow_user_terms,true}], Options)
	end;

%% Runs one module for an app (interactive)
run(App, Mod) when is_atom(App), is_atom(Mod) ->
    run_test({atom_to_list(App),Mod}, 
	     [{suite,Mod}], 
	     [interactive]).

%% run/3
%% Run one module for an app with Opts
run(App, Mod, Opts) when is_atom(App),
			 is_atom(Mod),
			 is_list(Opts) ->
    Options = check_test_get_opts(App, Opts),
    run_test({atom_to_list(App),Mod},
	     [{suite,Mod}], Options);

%% Run multiple modules with Opts
run(App, Mods, Opts) when is_atom(App),
			  is_list(Mods),
			  is_list(Opts) ->
    run_some([{App,M} || M <- Mods], Opts);

%% Runs one test case in a module.
run(App, Mod, Case) when is_atom(App),
			 is_atom(Mod),
			 is_atom(Case) ->
    Options = check_test_get_opts(App, []),
    Args = [{suite,Mod},{testcase,Case}],
    run_test(atom_to_list(App), Args, Options);

%% Runs one or more groups in a module.
run(App, Mod, Grs={group,_Groups}) when is_atom(App),
					is_atom(Mod) ->
    Options = check_test_get_opts(App, []),
    Args = [{suite,Mod},Grs],
    run_test(atom_to_list(App), Args, Options);

%% Runs one or more test cases in a module.
run(App, Mod, TCs={testcase,_Cases}) when is_atom(App),
					  is_atom(Mod) ->
    Options = check_test_get_opts(App, []),
    Args = [{suite,Mod},TCs],
    run_test(atom_to_list(App), Args, Options).

%% run/4
%% Run one test case in a module with Options.
run(App, Mod, Case, Opts) when is_atom(App), 
			       is_atom(Mod), 
			       is_atom(Case), 
			       is_list(Opts) ->
    Options = check_test_get_opts(App, Opts),
    Args = [{suite,Mod},{testcase,Case}],
    run_test(atom_to_list(App), Args, Options);

%% Run one or more test cases in a module with Options.
run(App, Mod, {testcase,Cases}, Opts) when is_atom(App), 
					   is_atom(Mod) ->
    run(App, Mod, Cases, Opts);
run(App, Mod, Cases, Opts) when is_atom(App), 
				is_atom(Mod),
				is_list(Cases),
				is_list(Opts) ->
    Options = check_test_get_opts(App, Opts),
    Args = [{suite,Mod},Cases],
    run_test(atom_to_list(App), Args, Options);

%% Run one or more test cases in a group.
run(App, Mod, Gr={group,_Group}, {testcase,Cases}) when is_atom(App),
							is_atom(Mod) ->
    run(App, Mod, Gr, Cases, [batch]);


%% Run one or more groups in a module with Options.
run(App, Mod, Grs={group,_Groups}, Opts) when is_atom(App), 
					      is_atom(Mod),
					      is_list(Opts) ->
    Options = check_test_get_opts(App, Opts),
    Args = [{suite,Mod},Grs],
    run_test(atom_to_list(App), Args, Options).

%% run/5
%% Run one or more test cases in a group with Options.
run(App, Mod, Group, Cases, Opts) when is_atom(App), 
				       is_atom(Mod),
				       is_list(Opts) ->
    Group1 = if is_tuple(Group) -> Group; true -> {group,Group} end,
    Cases1 = if is_tuple(Cases) -> Cases; true -> {testcase,Cases} end,
    Options = check_test_get_opts(App, Opts),
    Args = [{suite,Mod},Group1,Cases1],
    run_test(atom_to_list(App), Args, Options).

%% run_category/1
run_category(TestCategory) when is_atom(TestCategory) ->
    run_category(TestCategory, [batch]).

%% run_category/2
run_category(TestCategory, Opts) when is_atom(TestCategory),
				      is_list(Opts) ->
    case ts:tests(TestCategory) of
	[] ->
	    {error, no_tests_available};
	Apps ->
	    Opts1 = [{test_category,TestCategory} | Opts],
	    run_some(Apps, Opts1)
    end;

run_category(Apps, TestCategory) when is_atom(TestCategory) ->
    run_category(Apps, TestCategory, [batch]).

%% run_category/3
run_category(App, TestCategory, Opts) ->
    Apps = if is_atom(App) -> [App];
	      is_list(App) -> App
	   end,
    Opts1 = [{test_category,TestCategory} | Opts],
    run_some(Apps, Opts1).

%%-----------------------------------------------------------------
%% Functions kept for backwards compatibility

bench() ->
    run_category(bench, []).
bench(Opts) when is_list(Opts) ->
    run_category(bench, Opts);
bench(App) ->
    run_category(App, bench, []).
bench(App, Opts) when is_atom(App) ->
    run_category(App, bench, Opts);
bench(Apps, Opts) when is_list(Apps) ->
    run_category(Apps, bench, Opts).

benchmarks() ->
    tests(bench).

smoke_test() ->
    run_category(smoke, []).
smoke_test(Opts) when is_list(Opts) ->
    run_category(smoke, Opts);
smoke_test(App) ->
    run_category(App, smoke, []).
smoke_test(App, Opts) when is_atom(App) ->
    run_category(App, smoke, Opts);
smoke_test(Apps, Opts) when is_list(Apps) ->
    run_category(Apps, smoke, Opts).

smoke_tests() ->
    tests(smoke).

%%-----------------------------------------------------------------

is_list_of_suites(List) ->
    lists:all(fun(Suite) ->
		      S = if is_atom(Suite) -> atom_to_list(Suite);
			     true -> Suite
			  end,
		      try lists:last(string:lexemes(S,"_")) of
			  "SUITE" -> true;
			  "suite" -> true;
			  _ -> false
		      catch
			  _:_ -> false
		      end
	      end, List).	

%% Create a spec to skip all SUITES, this is used when the application
%% to be tested is not part of the OTP release to be tested.
create_skip_spec(App, SuitesToSkip) ->
    {ok,Cwd} = file:get_cwd(),
    AppString = atom_to_list(App),
    Specname = AppString++"_skip.spec",
    {ok,D} = file:open(filename:join([filename:dirname(Cwd),
				      AppString++"_test",Specname]),
		       [write]),
    TestDir = "\"../"++AppString++"_test\"",
    io:format(D,"{suites, "++TestDir++", all}.~n",[]),
    io:format(D,"{skip_suites, "++TestDir++", ~w, \"Skipped as application"
	      " is not in path!\"}.",[SuitesToSkip]),
    Specname.

%% Check testspec for App to be valid and get possible options
%% from the list.
check_test_get_opts(App, Opts) ->
    validate_test(App),
    Mode = configmember(batch, {batch, interactive}, Opts),
    Vars = configvars(Opts),
    Trace = get_config(trace,Opts),
    ConfigPath = get_config(config,Opts),
    KeepTopcase = configmember(keep_topcase, {keep_topcase,[]}, Opts),
    Cover = configcover(App,Opts),
    lists:flatten([Vars,Mode,Trace,KeepTopcase,Cover,ConfigPath]).
    
to_erlang_term(Atom) ->
    String = atom_to_list(Atom),
    {ok, Tokens, _} = erl_scan:string(lists:append([String, ". "])),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

%% Validate that Testspec really is a testspec,
%% and exit if not.
validate_test(Testspec) ->
    case lists:member(Testspec, tests()) of
	true ->
	    ok;
	false ->
	    io:format("This testspec does not seem to be "
		      "available.~n Please try ts:tests() "
		      "to see available tests.~n"),
	    exit(self(), {error, test_not_available})
    end.
    
configvars(Opts) ->
    case lists:keysearch(vars, 1, Opts) of
	{value, {vars, List}} ->
	    List0 = special_vars(Opts),
	    Key = fun(T) -> element(1,T) end,
	    DelDupList = 
		lists:filter(fun(V) -> 
				     case lists:keysearch(Key(V),1,List0) of
					 {value,_} -> false;
					 _ -> true
				     end
			     end, List),
	    {vars, [List0|DelDupList]};
	_ ->
	    {vars, special_vars(Opts)}
    end.

%% Allow some shortcuts in the options...
special_vars(Opts) ->
    SpecVars =
	case lists:member(verbose, Opts) of
	    true ->
		[{verbose, 1}];
	    false ->
		case lists:keysearch(verbose, 1, Opts) of
		    {value, {verbose, Lvl}} ->
			[{verbose, Lvl}];
		    _ ->
			[{verbose, 0}]
		end
	end,
    SpecVars1 =
	case lists:keysearch(diskless, 1, Opts) of
	    {value,{diskless, true}} ->
		[{diskless, true} | SpecVars];
	    _ ->
		SpecVars
	end,
    case lists:keysearch(testcase_callback, 1, Opts) of
	{value,{testcase_callback, CBM, CBF}} ->
	    [{ts_testcase_callback, {CBM,CBF}} | SpecVars1];
	{value,{testcase_callback, CB}} ->
	    [{ts_testcase_callback, CB} | SpecVars1];
	_ ->
	    SpecVars1
    end.

get_config(Key,Config) ->
    case lists:keysearch(Key,1,Config) of
	{value,Value} -> Value;
	false -> []
    end.

configcover(Testspec,[cover|_]) ->
    {cover,Testspec,default_coverfile(Testspec),overview};
configcover(Testspec,[cover_details|_]) ->
    {cover,Testspec,default_coverfile(Testspec),details};
configcover(Testspec,[{cover,File}|_]) ->
    {cover,Testspec,File,overview};
configcover(Testspec,[{cover_details,File}|_]) ->
    {cover,Testspec,File,details};
configcover(Testspec,[_H|T]) ->
    configcover(Testspec,T);
configcover(_Testspec,[]) ->
    [].

default_coverfile(Testspec) ->
    {ok,Cwd} = file:get_cwd(),
    CoverFile = filename:join([filename:dirname(Cwd), 
			       atom_to_list(Testspec)++"_test",
			       atom_to_list(Testspec)++".cover"]),
    case filelib:is_file(CoverFile) of
	true ->
	    CoverFile;
	false ->
	    none
    end.

configmember(Member, {True, False}, Config) ->
    case lists:member(Member, Config) of
	true ->
	    True;
	false ->
	    False
    end.


check_for_cross_cover_analysis_flag(Config) ->
    check_for_cross_cover_analysis_flag(Config,false,false).
check_for_cross_cover_analysis_flag([cover|Config],false,false) ->
    check_for_cross_cover_analysis_flag(Config,overview,false);
check_for_cross_cover_analysis_flag([cover|_Config],false,true) ->
    overview;
check_for_cross_cover_analysis_flag([cover_details|Config],false,false) ->
    check_for_cross_cover_analysis_flag(Config,details,false);
check_for_cross_cover_analysis_flag([cover_details|_Config],false,true) ->
    details;
check_for_cross_cover_analysis_flag([cross_cover_analysis|Config],false,_) ->
    check_for_cross_cover_analysis_flag(Config,false,true);
check_for_cross_cover_analysis_flag([cross_cover_analysis|_Config],Level,_) ->
    Level;
check_for_cross_cover_analysis_flag([_|Config],Level,CrossFlag) ->
    check_for_cross_cover_analysis_flag(Config,Level,CrossFlag);
check_for_cross_cover_analysis_flag([],_,_) ->
    false.


%% Returns all available apps.
tests() ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:specs(Cwd).

%% Returns all apps that provide tests in the given test category
tests(main) ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:specs(Cwd);
tests(bench) ->
    ts_benchmark:benchmarks();
tests(TestCategory) ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:specialized_specs(Cwd, atom_to_list(TestCategory)).
    
%% Returns a list of available test suites for App.
suites(App) ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:suites(Cwd, atom_to_list(App)).

%% Returns all available test categories for App
categories(App) ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:test_categories(Cwd, atom_to_list(App)).

%% 
%% estone/0, estone/1
%% Opts = same as Opts or Config for the run(...) function, 
%% e.g. [batch]
%% 
estone() -> run(emulator,estone_SUITE).
estone(Opts) when is_list(Opts) -> run(emulator,estone_SUITE,Opts).

%% 
%% cross_cover_analyse/1
%% Level = details | overview
%% Can be called on any node after a test (with cover) is
%% completed. The node's current directory must be the same as when
%% the tests were run.
%%
cross_cover_analyse([Level]) ->
    cross_cover_analyse(Level);
cross_cover_analyse(Level) ->
    Apps = get_last_app_tests(),
    test_server_ctrl:cross_cover_analyse(Level,Apps).

get_last_app_tests() ->
    AllTests = filelib:wildcard(filename:join(["*","*_test.logs"])),
    {ok,RE} = re:compile("^[^/]*/[^\.]*\.(.*)_test\.logs$"),
    get_last_app_tests(AllTests,RE,[]).

get_last_app_tests([Dir|Dirs],RE,Acc) ->
    NewAcc =
	case re:run(Dir,RE,[{capture,all,list}]) of
	    {match,[Dir,AppStr]} ->
		Dir1 = filename:dirname(Dir), % cover logs in ct_run.<t> dir
		App = list_to_atom(AppStr),
		case lists:keytake(App,1,Acc) of
		    {value,{App,LastDir},Rest} ->
			if Dir1 > LastDir ->
				[{App,Dir1}|Rest];
			   true ->
				Acc
			end;
		    false ->
			[{App,Dir1} | Acc]
		end;
	    _ ->
		Acc
	end,
    get_last_app_tests(Dirs,RE,NewAcc);
get_last_app_tests([],_,Acc) ->
    Acc.

%%% Implementation.

check_and_run(Fun) ->
    case file:consult(?variables) of
	{ok, Vars} ->
	    check_and_run(Fun, Vars);
	{error, Error} when is_atom(Error) ->
	    {error, not_installed};
	{error, Reason} ->
	    {error, {bad_installation, file:format_error(Reason)}}
    end.

check_and_run(Fun, Vars) ->
    Platform = ts_install:platform_id(Vars),
    case lists:keysearch(platform_id, 1, Vars) of
	{value, {_, Platform}} ->
	    case catch apply(Fun, [Vars]) of
		{'EXIT', Reason} ->
		    exit(Reason);
		Other ->
		    Other
	    end;
	{value, {_, OriginalPlatform}} ->
	    io:format("These test suites were installed for '~s'.\n",
		      [OriginalPlatform]),
	    io:format("But the current platform is '~s'.\nPlease "
		      "install for this platform before running "
		      "any tests.\n", [Platform]),
	    {error, inconsistent_platforms};
	false ->
	    {error, {bad_installation, no_platform}}
    end.

run_test(File, Args, Options) ->
    check_and_run(fun(Vars) -> run_test(File, Args, Options, Vars) end).

run_test(File, Args, Options, Vars) ->
    ts_run:run(File, Args, Options, Vars).


%% This module provides some convenient shortcuts to running
%% the test server from within a started Erlang shell.
%% (This are here for backwards compatibility.)
%%
%% r()
%% r(Opts)
%% r(SpecOrMod)
%% r(SpecOrMod, Opts)
%% r(Mod, Case)
%% r(Mod, Case, Opts)
%%	Each of these functions starts the test server if it
%%	isn't already running, then runs the test case(s) selected
%%	by the aguments.
%%      SpecOrMod can be a module name or the name of a test spec file,
%%      with the extension .spec or .spec.OsType. The module Mod will 
%%	be reloaded before running the test cases.
%%      Opts = [Opt],
%%      Opt = {Cover,AppOrCoverFile} | {Cover,App,CoverFile}
%%      Cover = cover | cover_details
%%      AppOrCoverFile = App | CoverFile
%%      App = atom(), an application name
%%      CoverFile = string(), name of a cover file 
%%                  (see doc of test_server_ctrl:cover/2/3)
%%
%% i()
%%	Shows information about the jobs being run, by dumping
%%	the process information for the test_server.
%%
%% l(Mod)
%%	This function reloads a module just like c:l/1, but works
%%	even for a module in one of the sticky library directories
%%	(for instance, lists can be reloaded).

%% Runs all tests cases in the current directory.

r() ->
    r([]).
r(Opts) when is_list(Opts), is_atom(hd(Opts)) ->
    ensure_ts_started(Opts),
    test_server_ctrl:add_dir("current_dir", ".");

%% Checks if argument is a spec file or a module 
%% (spec file must be named "*.spec" or "*.spec.OsType")
%% If module, reloads module and runs all test cases in it.
%% If spec, runs all test cases in it.

r(SpecOrMod) ->
    r(SpecOrMod,[]).
r(SpecOrMod,Opts) when is_list(Opts) ->
    ensure_ts_started(Opts),
    case filename:extension(SpecOrMod) of
	[] ->
	    l(SpecOrMod),
	    test_server_ctrl:add_module(SpecOrMod);
	".spec" -> 
	    test_server_ctrl:add_spec(SpecOrMod);
	_ ->
	    Spec2 = filename:rootname(SpecOrMod),
	    case filename:extension(Spec2) of
		".spec" ->
		    %% *.spec.Type
		    test_server_ctrl:add_spec(SpecOrMod);
		_ ->
		    {error, unknown_filetype}
	    end		    
    end;

%% Reloads the given module and runs the given test case in it.

r(Mod, Case) ->
    r(Mod,Case,[]).
r(Mod, Case, Opts) ->
    ensure_ts_started(Opts),
    l(Mod),
    test_server_ctrl:add_case(Mod, Case).

%% Shows information about the jobs being run.

i() ->
    ensure_ts_started([]),
    hformat("Job", "Current", "Total", "Success", "Failed", "Skipped"),
    i(test_server_ctrl:jobs()).
 
i([{Name, Pid}|Rest]) when is_pid(Pid) ->
    {dictionary, PI} = process_info(Pid, dictionary),
    {value, {_, CaseNum}} = lists:keysearch(test_server_case_num, 1, PI),
    {value, {_, Cases}} = lists:keysearch(test_server_cases, 1, PI),
    {value, {_, Failed}} = lists:keysearch(test_server_failed, 1, PI),
    {value, {_, {UserSkipped,AutoSkipped}}} = lists:keysearch(test_server_skipped, 1, PI),
    {value, {_, Ok}} = lists:keysearch(test_server_ok, 1, PI),
    nformat(Name, CaseNum, Cases, Ok, Failed, UserSkipped+AutoSkipped),
    i(Rest);
i([]) ->
    ok.
 
hformat(A1, A2, A3, A4, A5, A6) ->
    io:format("~-20s ~8s ~8s ~8s ~8s ~8s~n", [A1,A2,A3,A4,A5,A6]).
 
nformat(A1, A2, A3, A4, A5, A6) ->
    io:format("~-20s ~8w ~8w ~8w ~8w ~8w~n", [A1,A2,A3,A4,A5,A6]).

%% Force load of a module even if it is in a sticky directory.

l(Mod) ->
    case do_load(Mod) of
	{error, sticky_directory} ->
	    Dir = filename:dirname(code:which(Mod)),
	    code:unstick_dir(Dir),
	    do_load(Mod),
	    code:stick_dir(Dir);
	X ->
	    X
    end.


ensure_ts_started(Opts) ->
    Pid = case whereis(test_server_ctrl) of
	      undefined ->
		  test_server_ctrl:start();
	      P when is_pid(P) ->
		  P
	  end,
    case Opts of
	[{Cover,AppOrCoverFile}] when Cover==cover; Cover==cover_details ->
	    test_server_ctrl:cover(AppOrCoverFile,cover_type(Cover));
	[{Cover,App,CoverFile}]  when Cover==cover; Cover==cover_details ->
	    test_server_ctrl:cover(App,CoverFile,cover_type(Cover));
	_ ->
	    ok
    end,
    Pid.

cover_type(cover) -> overview;
cover_type(cover_details) -> details.

do_load(Mod) ->
    code:purge(Mod),
    code:load_file(Mod).


compile_testcases() ->
    compile_datadirs("../*/*_data").

compile_testcases(App) when is_atom(App) ->
    compile_testcases([App]);
compile_testcases([App | T]) ->
    compile_datadirs(io_lib:format("../~s_test/*_data", [App])),
    compile_testcases(T);
compile_testcases([]) ->
    ok.

compile_datadirs(DataDirs) ->
    {ok,Variables} = file:consult("variables"),

    lists:foreach(fun(Dir) ->
			  ts_lib:make_non_erlang(Dir, Variables)
		  end,
		  filelib:wildcard(DataDirs)).
