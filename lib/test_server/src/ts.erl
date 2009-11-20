%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

-export([run/0, run/1, run/2, run/3, run/4,
	 clean/0, clean/1,
	 tests/0, tests/1,
	 install/0, install/1, install/2, index/0,
	 estone/0, estone/1,
	 cross_cover_analyse/1,
	 help/0]).
-export([i/0, l/1, r/0, r/1, r/2, r/3]).

%%%----------------------------------------------------------------------
%%% This module, ts, is the interface to all of the functionality of
%%% the TS framework.  The picture below shows the relationship of
%%% the modules:
%%%
%%%       +-- ts_install --+------  ts_autoconf_win32
%%%       |                |
%%%       |                +------  ts_autoconf_vxworks
%%%       |
%%% ts ---+                +------  ts_erl_config
%%%       |                |				     ts_lib
%%%       |                +------  ts_make
%%%       |                |
%%%       +-- ts_run  -----+
%%%                        |	    			     ts_filelib
%%%                        +------  ts_make_erl
%%%                        |
%%%                        +------  ts_reports (indirectly)
%%%       
%%%       
%%%
%%% The modules ts_lib and ts_filelib contains utilities used by
%%% the other modules.
%%%
%%% Module		 Description
%%% ------		 -----------
%%% ts			 Frontend to the test server framework.  Contains all
%%%			 interface functions.
%%% ts_install		 Installs the test suite.  On Unix, `autoconf' is
%%%			 is used; on Windows, ts_autoconf_win32 is used,
%%%                      on VxWorks, ts_autoconf_vxworks is used.
%%%			 The result is written to the file `variables'.
%%% ts_run		 Supervises running of the tests.
%%% ts_autconf_win32	 An `autoconf' for Windows.
%%% ts_autconf_cross_env `autoconf' for other platforms (cross environment)
%%% ts_erl_config	 Finds out information about the Erlang system,
%%%			 for instance the location of erl_interface.
%%%			 This works for either an installed OTP or an Erlang
%%%			 system running from Clearcase.
%%% ts_make		 Interface to run the `make' program on Unix
%%%			 and other platforms.
%%% ts_make_erl		 A corrected version of the standar Erlang module
%%%			 make (used for rebuilding test suites).
%%% ts_reports		 Generates index pages in HTML, providing a summary
%%%			 of the tests run.
%%% ts_lib		 Miscellanous utility functions, each used by several
%%%			 other modules.
%%%----------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").
-include("ts.hrl").

-define(
   install_help,
   [
    "  ts:install()      - Install TS for local target with no Options.\n"
    "  ts:install([Options])\n",
    "                    - Install TS for local target with Options\n"
    "  ts:install({Architecture, Target_name})\n",
    "                    - Install TS for a remote target architecture.\n",
    "                      and target network name (e.g. {vxworks_cpu32, sauron}).\n",
    "  ts:install({Architecture, Target_name}, [Options])\n",
    "                    - Install TS as above, and with Options.\n",
    "\n",
    "Installation options supported:\n",
    "  {longnames, true} - Use fully qualified hostnames\n",
    "  {hosts, [HostList]}\n"
    "                    - Use theese hosts for distributed testing.\n"
    "  {verbose, Level}  - Sets verbosity level for TS output (0,1,2), 0 is\n"
    "                      quiet(default).\n"
    "  {slavetargets, SlaveTarges}\n"
    "                    - Available hosts for starting slave nodes for\n"
    "                      platforms which cannot have more than one erlang\n"
    "                      node per host.\n"
    "  {crossroot, TargetErlRoot}\n"
    "                    - Erlang root directory on target host\n"
    "                      Mandatory for remote targets\n"
    "  {master, {MasterHost, MasterCookie}}\n"
    "                    - Master host and cookie for targets which are\n"
    "                      started as slave nodes (i.e. OSE/Delta targets\n"
    "                      erl_boot_server must be started on master before\n"
    "                      test is run.\n"
    "                      Optional, default is controller host and then\n"
    "                      erl_boot_server is started autmatically\n"
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
	 "  All above run functions can have the additional Options argument\n",
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
	 "\n",
	 "Supported trace information elements\n",
	 "  {tp | tpl, Mod, [] | match_spec()}\n",
	 "  {tp | tpl, Mod, Func, [] | match_spec()}\n",
	 "  {tp | tpl, Mod, Func, Arity, [] | match_spec()}\n",
	 "  {ctp | ctpl, Mod}\n",
	 "  {ctp | ctpl, Mod, Func}\n",
	 "  {ctp | ctpl, Mod, Func, Arity}\n",
	 "\n",
	 "Support functions\n",
	 "  ts:tests()        - Shows all available families of tests.\n",
	 "  ts:tests(Spec)    - Shows all available test modules in Spec,\n",
	 "                      i.e. ../Spec_test/*_SUITE.erl\n",
	 "  ts:index()        - Updates local index page.\n",
	 "  ts:clean()        - Cleans up all but the last tests run.\n",
	 "  ts:clean(all)     - Cleans up all test runs found.\n",
	 "  ts:estone()       - Run estone_SUITE in kernel application with\n"
	 "                      no run options\n",
	 "  ts:estone(Opts)   - Run estone_SUITE in kernel application with\n"
	 "                      the given run options\n",
	 "  ts:cross_cover_analyse(Level)\n"
	 "                    - Used after ts:run with option cover or \n"
	 "                      cover_details. Analyses modules specified in\n"
	 "                      cross.cover.\n"
	 "                      Level can be 'overview' or 'details'.\n",
	 " \n"
	 "Installation (already done):\n"
	],
    show_help([H,?install_help]).

show_help(H) ->
    io:put_chars(lists:flatten(H)).


%% Installs tests.
install() ->
    ts_install:install(install_local,[]).
install({Architecture, Target_name})  ->
    ts_install:install({ts_lib:maybe_atom_to_list(Architecture), 
			ts_lib:maybe_atom_to_list(Target_name)}, []);
install(Options) when is_list(Options) ->
    ts_install:install(install_local,Options).
install({Architecture, Target_name}, Options) when is_list(Options)->
    ts_install:install({ts_lib:maybe_atom_to_list(Architecture), 
			ts_lib:maybe_atom_to_list(Target_name)}, Options).

%% Updates the local index page.

index() ->
    check_and_run(fun(_Vars) -> ts_reports:make_index(), ok end).

%%
%% clean(all)
%% Deletes all logfiles.
%%
clean(all) ->
    delete_files(filelib:wildcard("*" ++ ?logdir_ext)).

%% clean/0
%%
%% Cleans up run logfiles, all but the last run.
clean() ->
    clean1(filelib:wildcard("*" ++ ?logdir_ext)).

clean1([Dir|Dirs]) ->
    List0 = filelib:wildcard(filename:join(Dir, "run.*")),
    case lists:reverse(lists:sort(List0)) of
	[] -> ok;
	[_Last|Rest] -> delete_files(Rest)
    end,
    clean1(Dirs);
clean1([]) -> ok.

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
    run_test(File, ["SPEC current.spec NAME ",File], Options);

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
%% Runs one test spec with Options
run(Testspec, Config) when is_atom(Testspec), is_list(Config) ->
    Options=check_test_get_opts(Testspec, Config),
    File=atom_to_list(Testspec),
    run_test(File, ["SPEC current.spec NAME ", File], Options);
%% Runs one module in a spec (interactive)
run(Testspec, Mod) when is_atom(Testspec), is_atom(Mod) ->
    run_test({atom_to_list(Testspec), Mod}, 
	     ["SPEC current.spec NAME ", atom_to_list(Mod)], 
	     [interactive]).

%% run/3
%% Run one module in a spec with Config
run(Testspec,Mod,Config) when is_atom(Testspec), is_atom(Mod), is_list(Config) ->
    Options=check_test_get_opts(Testspec, Config),
    run_test({atom_to_list(Testspec), Mod},
	     ["SPEC current.spec NAME ", atom_to_list(Mod)], 
	     Options);

%% Runs one testcase in a module.
run(Testspec, Mod, Case) when is_atom(Testspec), is_atom(Mod), is_atom(Case) ->
    Options=check_test_get_opts(Testspec, []),
    Args = ["CASE ",atom_to_list(Mod)," ",atom_to_list(Case)],
    run_test(atom_to_list(Testspec), Args, Options).

%% run/4
%% Run one testcase in a module with Options.
run(Testspec, Mod, Case, Config) when is_atom(Testspec), is_atom(Mod), is_atom(Case), is_list(Config) ->
    Options=check_test_get_opts(Testspec, Config),
    Args = ["CASE ",atom_to_list(Mod), " ",atom_to_list(Case)],
    run_test(atom_to_list(Testspec), Args, Options).

%% Check testspec to be valid and get possible Options
%% from the config.
check_test_get_opts(Testspec, Config) ->
    validate_test(Testspec),
    Mode = configmember(batch, {batch, interactive}, Config),
    Vars = configvars(Config),
    Trace = configtrace(Config),
    KeepTopcase = configmember(keep_topcase, {keep_topcase,[]}, Config),
    Cover = configcover(Testspec,Config),
    lists:flatten([Vars,Mode,Trace,KeepTopcase,Cover]).
    
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

configtrace(Config) ->
    case lists:keysearch(trace,1,Config) of
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
    test_server_ctrl:cross_cover_analyse(Level).


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


delete_files([]) -> ok;
delete_files([Item|Rest]) ->
    case file:delete(Item) of
	ok ->
	    delete_files(Rest);
	{error,eperm} ->
	    file:change_mode(Item, 8#777),
	    delete_files(filelib:wildcard(filename:join(Item, "*"))),
	    file:del_dir(Item),
	    ok;
	{error,eacces} ->
	    %% We'll see about that!
	    file:change_mode(Item, 8#777),
	    case file:delete(Item) of
		ok -> ok;
		{error,_} ->
		    erlang:yield(),
		    file:change_mode(Item, 8#777),
		    file:delete(Item),
		    ok
	    end;
	{error,_} -> ok
    end,
    delete_files(Rest).


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
