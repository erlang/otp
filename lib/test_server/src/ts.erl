%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
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
%%% File    : ts.erl
%%% Purpose : Frontend for running tests.
%%%-------------------------------------------------------------------

-module(ts).

-export([run/0, run/1, run/2, run/3, run/4, run/5,
	 tests/0, tests/1,
	 install/0, install/1,
	 bench/0, bench/1, bench/2, benchmarks/0,
	 smoke_test/0, smoke_test/1,smoke_test/2, smoke_tests/0,
	 estone/0, estone/1,
	 cross_cover_analyse/1,
	 compile_testcases/0, compile_testcases/1,
	 help/0]).
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
    "  ts:install()           - Install TS with no Options.\n"
    "  ts:install([Options])  - Install TS with Options\n"
    "\n",
    "Installation options supported:\n",
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
    H = ["TS is not installed yet.  To install use:\n\n"],
    show_help([H,?install_help]);
help(installed) ->
    H = ["Run functions:\n",
	 "  ts:run()          - Run all available tests.\n",
	 "  ts:run(Spec)      - Run all tests in given test spec file.\n",
	 "                      The spec file is actually ../*_test/Spec.spec\n",
	 "  ts:run([Specs])   - Run all tests in all given test spec files.\n",
	 "  ts:run(Spec, Mod) - Run a single test suite.\n",
	 "  ts:run(Spec, Mod, Case)\n",
	 "                    - Run a single test case.\n",
	 "  All above run functions can have an additional Options argument\n",
	 "  which is a list of options.\n",
	 "\n",
	 "Run options supported:\n",
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
	 "Supported trace information elements\n",
	 "  {tp | tpl, Mod, [] | match_spec()}\n",
	 "  {tp | tpl, Mod, Func, [] | match_spec()}\n",
	 "  {tp | tpl, Mod, Func, Arity, [] | match_spec()}\n",
	 "  {ctp | ctpl, Mod}\n",
	 "  {ctp | ctpl, Mod, Func}\n",
	 "  {ctp | ctpl, Mod, Func, Arity}\n",
	 "\n",
	 "Support functions:\n",
	 "  ts:tests()        - Shows all available families of tests.\n",
	 "  ts:tests(Spec)    - Shows all available test modules in Spec,\n",
	 "                      i.e. ../Spec_test/*_SUITE.erl\n",
	 "  ts:estone()       - Run estone_SUITE in kernel application with\n"
	 "                      no run options\n",
	 "  ts:estone(Opts)   - Run estone_SUITE in kernel application with\n"
	 "                      the given run options\n",
	 "  ts:cross_cover_analyse(Level)\n"
	 "                    - Used after ts:run with option cover or \n"
	 "                      cover_details. Analyses modules specified with\n"
	 "                      a 'cross' statement in the cover spec file.\n"
	 "                      Level can be 'overview' or 'details'.\n",
	 "  ts:compile_testcases()~n"
	 "  ts:compile_testcases(Apps)~n"
	 "                    - Compile all testcases for usage in a cross ~n"
	 "                      compile environment."
	 " \n"
	 "Benchmark functions:\n"
	 "  ts:benchmarks()   - Get all available families of benchmarks\n"
	 "  ts:bench()        - Runs all benchmarks\n"
	 "  ts:bench(Spec)    - Runs all benchmarks in the given spec file.\n"
	 "                      The spec file is actually ../*_test/Spec_bench.spec\n\n"
	 "                      ts:bench can take the same Options argument as ts:run.\n"
	 "Smoke test functions:\n"
	 "  ts:smoke_tests()  - Get all available families of smoke tests\n"
	 "  ts:smoke_test()   - Runs all smoke tests\n"
	 "  ts:smoke_test(Spec)\n"
	 "                    - Runs all smoke tests in the given spec file.\n"
	 "                      The spec file is actually ../*_test/Spec_smoke.spec\n\n"
	 "                      ts:smoke_test can take the same Options argument as ts:run.\n"
	 "\n"
	 "Installation (already done):\n"
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
run_some([{Spec,Mod}|Specs], Opts) ->
    case run(Spec, Mod, Opts) of
	ok -> ok;
	Error -> io:format("~p: ~p~n",[{Spec,Mod},Error])
    end,
    run_some(Specs, Opts);
run_some([Spec|Specs], Opts) ->
    case run(Spec, Opts) of
	ok -> ok;
	Error -> io:format("~p: ~p~n",[Spec,Error])
    end,
    run_some(Specs, Opts).

%% Runs one test spec (interactive).
run(Testspec) when is_atom(Testspec) ->
    Options=check_test_get_opts(Testspec, []),
    File = atom_to_list(Testspec),
    run_test(File, [{spec,[File++".spec"]}], Options);

%% This can be used from command line, e.g.
%% erl -s ts run all_tests <config>
%% When using the all_tests flag and running with cover, one can also
%% use the cross_cover_analysis flag.
run([all_tests|Config0]) ->
    AllAtomsFun = fun(X) when is_atom(X) -> true; 
		     (_) -> false 
		  end,
    Config1 = 
	case lists:all(AllAtomsFun,Config0) of
	    true ->
		%% Could be from command line
		lists:map(fun(Conf)->to_erlang_term(Conf) end,Config0)--[batch];
	    false ->
		Config0--[batch]
	end,
    %% Make sure there is exactly one occurence of 'batch'
    Config2 = [batch|Config1],

    R = run(tests(),Config2),

    case check_for_cross_cover_analysis_flag(Config2) of
	false ->
	    ok;
	Level ->
	    cross_cover_analyse(Level)
    end,

    R;

%% ts:run(ListOfTests)
run(List) when is_list(List) ->
    run(List, [batch]).

run(List, Opts) when is_list(List), is_list(Opts) ->
    run_some(List, Opts);

%% run/2
%% Runs one test spec with list of suites or with options
run(Testspec, ModsOrConfig) when is_atom(Testspec),
				 is_list(ModsOrConfig) ->
    case is_list_of_suites(ModsOrConfig) of
	false ->
	    run(Testspec, {config_list,ModsOrConfig});
	true ->
	    run_some([{Testspec,M} || M <- ModsOrConfig],
		     [batch])
    end;
run(Testspec, {config_list,Config}) ->
    Options=check_test_get_opts(Testspec, Config),
    IsSmoke=proplists:get_value(smoke,Config),
    File=atom_to_list(Testspec),
    WhatToDo =
	case Testspec of
	    %% Known to exist but fails generic tests below
	    emulator -> test;
	    system -> test;
	    erl_interface -> test;
	    epmd -> test;
	    _ ->
		case code:lib_dir(Testspec) of
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
    Spec =
	case WhatToDo of
	    skip ->
		create_skip_spec(Testspec, tests(Testspec));
	    test when IsSmoke ->
		File++"_smoke.spec";
	    test ->
		File++".spec"
	end,
    run_test(File, [{spec,[Spec]}], Options);
%% Runs one module in a spec (interactive)
run(Testspec, Mod) when is_atom(Testspec), is_atom(Mod) ->
    run_test({atom_to_list(Testspec),Mod}, 
	     [{suite,Mod}], 
	     [interactive]).

%% run/3
%% Run one module in a spec with Config
run(Testspec, Mod, Config) when is_atom(Testspec),
				is_atom(Mod),
				is_list(Config) ->
    Options=check_test_get_opts(Testspec, Config),
    run_test({atom_to_list(Testspec),Mod},
	     [{suite,Mod}], Options);
%% Run multiple modules with Config
run(Testspec, Mods, Config) when is_atom(Testspec),
				 is_list(Mods),
				 is_list(Config) ->
    run_some([{Testspec,M} || M <- Mods], Config);
%% Runs one test case in a module.
run(Testspec, Mod, Case) when is_atom(Testspec),
			      is_atom(Mod),
			      is_atom(Case) ->
    Options=check_test_get_opts(Testspec, []),
    Args = [{suite,Mod},{testcase,Case}],
    run_test(atom_to_list(Testspec), Args, Options);
%% Runs one or more groups in a module.
run(Testspec, Mod, Grs={group,_Groups}) when is_atom(Testspec),
					    is_atom(Mod) ->
    Options=check_test_get_opts(Testspec, []),
    Args = [{suite,Mod},Grs],
    run_test(atom_to_list(Testspec), Args, Options);
%% Runs one or more test cases in a module.
run(Testspec, Mod, TCs={testcase,_Cases}) when is_atom(Testspec),
					       is_atom(Mod) ->
    Options=check_test_get_opts(Testspec, []),
    Args = [{suite,Mod},TCs],
    run_test(atom_to_list(Testspec), Args, Options).

%% run/4
%% Run one test case in a module with Options.
run(Testspec, Mod, Case, Config) when is_atom(Testspec), 
				      is_atom(Mod), 
				      is_atom(Case), 
				      is_list(Config) ->
    Options=check_test_get_opts(Testspec, Config),
    Args = [{suite,Mod},{testcase,Case}],
    run_test(atom_to_list(Testspec), Args, Options);
%% Run one or more test cases in a module with Options.
run(Testspec, Mod, {testcase,Cases}, Config) when is_atom(Testspec), 
						  is_atom(Mod) ->
    run(Testspec, Mod, Cases, Config);
run(Testspec, Mod, Cases, Config) when is_atom(Testspec), 
				       is_atom(Mod),
				       is_list(Cases),
				       is_list(Config) ->
    Options=check_test_get_opts(Testspec, Config),
    Args = [{suite,Mod},Cases],
    run_test(atom_to_list(Testspec), Args, Options);
%% Run one or more groups in a module with Options.
run(Testspec, Mod, Grs={group,_Groups}, Config) when is_atom(Testspec), 
						     is_atom(Mod) ->
    Options=check_test_get_opts(Testspec, Config),
    Args = [{suite,Mod},Grs],
    run_test(atom_to_list(Testspec), Args, Options).

%% run/5
%% Run one or more test cases in a group with Options.
run(Testspec, Mod, Group, Cases, Config) when is_atom(Testspec), 
					      is_atom(Mod),
					      is_list(Config) ->
    Group1 = if is_tuple(Group) -> Group; true -> {group,Group} end,
    Cases1 = if is_tuple(Cases) -> Cases; true -> {testcase,Cases} end,
    Options=check_test_get_opts(Testspec, Config),
    Args = [{suite,Mod},Group1,Cases1],
    run_test(atom_to_list(Testspec), Args, Options).

is_list_of_suites(List) ->
    lists:all(fun(Suite) ->
		      S = if is_atom(Suite) -> atom_to_list(Suite);
			     true -> Suite
			  end,
		      try lists:last(string:tokens(S,"_")) of
			  "SUITE" -> true;
			  "suite" -> true;
			  _ -> false
		      catch
			  _:_ -> false
		      end
	      end, List).	

%% Create a spec to skip all SUITES, this is used when the application
%% to be tested is not part of the OTP release to be tested.
create_skip_spec(Testspec, SuitesToSkip) ->
    {ok,Cwd} = file:get_cwd(),
    TestspecString = atom_to_list(Testspec),
    Specname = TestspecString++"_skip.spec",
    {ok,D} = file:open(filename:join([filename:dirname(Cwd),
				      TestspecString++"_test",Specname]),
		       [write]),
    TestDir = "\"../"++TestspecString++"_test\"",
    io:format(D,"{suites, "++TestDir++", all}.~n",[]),
    io:format(D,"{skip_suites, "++TestDir++", ~w, \"Skipped as application"
	      " is not in path!\"}.",[SuitesToSkip]),
    Specname.

%% Check testspec to be valid and get possible Options
%% from the config.
check_test_get_opts(Testspec, Config) ->
    validate_test(Testspec),
    Mode = configmember(batch, {batch, interactive}, Config),
    Vars = configvars(Config),
    Trace = get_config(trace,Config),
    ConfigPath = get_config(config,Config),
    KeepTopcase = configmember(keep_topcase, {keep_topcase,[]}, Config),
    Cover = configcover(Testspec,Config),
    lists:flatten([Vars,Mode,Trace,KeepTopcase,Cover,ConfigPath]).
    
to_erlang_term(Atom) ->
    String = atom_to_list(Atom),
    {ok, Tokens, _} = erl_scan:string(lists:append([String, ". "])),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

%% Validate that a Testspec really is a testspec,
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
    
configvars(Config) ->
    case lists:keysearch(vars, 1, Config) of
	{value, {vars, List}} ->
	    List0 = special_vars(Config),
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
	    {vars, special_vars(Config)}
    end.

%% Allow some shortcuts in the Options...
special_vars(Config) ->
    SpecVars =
	case lists:member(verbose, Config) of
	    true ->
		[{verbose, 1}];
	    false ->
		case lists:keysearch(verbose, 1, Config) of
		    {value, {verbose, Lvl}} ->
			[{verbose, Lvl}];
		    _ ->
			[{verbose, 0}]
		end
	end,
    SpecVars1 =
	case lists:keysearch(diskless, 1, Config) of
	    {value,{diskless, true}} ->
		[{diskless, true} | SpecVars];
	    _ ->
		SpecVars
	end,
    case lists:keysearch(testcase_callback, 1, Config) of
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

%% Returns a list of available test suites.

tests() ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:specs(Cwd).

tests(Spec) ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:suites(Cwd, atom_to_list(Spec)).

%% Benchmark related functions

bench() ->
    bench([]).

bench(Opts) when is_list(Opts) ->
    bench(benchmarks(),Opts);
bench(Spec) ->
    bench([Spec],[]).

bench(Spec, Opts) when is_atom(Spec) ->
    bench([Spec],Opts);
bench(Specs, Opts) ->
    check_and_run(fun(Vars) -> ts_benchmark:run(Specs, Opts, Vars) end).

benchmarks() ->
    ts_benchmark:benchmarks().

smoke_test() ->
    smoke_test([]).

smoke_test(Opts) when is_list(Opts) ->
    smoke_test(smoke_tests(),Opts);
smoke_test(Spec) ->
    smoke_test([Spec],[]).

smoke_test(Spec, Opts) when is_atom(Spec) ->
    smoke_test([Spec],Opts);
smoke_test(Specs, Opts) ->
    run(Specs, [{smoke,true}|Opts]).

smoke_tests() ->
    {ok, Cwd} = file:get_cwd(),
    ts_lib:specialized_specs(Cwd,"smoke").

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
