%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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
%%
%% Test suite for the systools module.
%%
%%	The systools module is a wrapper for a number of modules that
%%	handle large parts of the release building functionality
%%	(e.g. checking app files, building a tar file, building
%%	release upgrad scripts.
%%


-module(systools_SUITE).

%-define(debug, true).

-include_lib("test_server/include/test_server.hrl").
-define(format(S, A), ok).
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-define(copydir, ?config(copy_dir, Config)).

-include_lib("kernel/include/file.hrl").

-export([all/0,suite/0,groups/0,init_per_group/2,end_per_group/2]).

-export([ script_options/1, normal_script/1, no_mod_vsn_script/1,
	  wildcard_script/1, variable_script/1,
	  abnormal_script/1, src_tests_script/1, crazy_script/1,
	  warn_shadow_script/1,
	  included_script/1, included_override_script/1,
	  included_fail_script/1, included_bug_script/1, exref_script/1]).
-export([ tar_options/1, normal_tar/1, no_mod_vsn_tar/1, variable_tar/1,
	  src_tests_tar/1, shadow_tar/1, var_tar/1,
	  exref_tar/1, link_tar/1, otp_9507/1]).
-export([ normal_relup/1, abnormal_relup/1, no_appup_relup/1,
	  bad_appup_relup/1, app_start_type_relup/1, otp_3065/1]).
-export([
	 otp_6226/1]).
-export([init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2]).

-import(lists, [foldl/3]).

-define(default_timeout, ?t:minutes(20)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [{group, script}, {group, tar}, {group, relup},
     {group, tickets}].

groups() -> 
    [{script, [],
      [script_options, normal_script, no_mod_vsn_script,
       wildcard_script, variable_script, abnormal_script,
       src_tests_script, crazy_script, warn_shadow_script,
       included_script, included_override_script,
       included_fail_script, included_bug_script, exref_script,
       otp_3065]},
     {tar, [],
      [tar_options, normal_tar, no_mod_vsn_tar, variable_tar,
       src_tests_tar, shadow_tar, var_tar,
       exref_tar, link_tar, otp_9507]},
     {relup, [],
      [normal_relup, abnormal_relup, no_appup_relup,
       bad_appup_relup, app_start_type_relup]},
     {tickets, [], [otp_6226]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_per_suite(Config) when is_list(Config) ->
    %% Make of copy of the data directory.
    DataDir = ?datadir,
    PrivDir = ?privdir,
    ?line CopyDir = fname(PrivDir, "datacopy"),
    ?line TarFile = fname(PrivDir, "datacopy.tgz"),
    ?line {ok, Tar} = erl_tar:open(TarFile, [write, compressed]),
    ?line ok = erl_tar:add(Tar, DataDir, CopyDir, [compressed]),
    ?line ok = erl_tar:close(Tar),
    ?line ok = erl_tar:extract(TarFile, [compressed]),
    ?line ok = file:delete(TarFile),

    %% Compile source files in the copy directory.
    ?line Sources = filelib:wildcard(fname([CopyDir,'*','*','*','*','*.erl'])),
    ?line lists:foreach(fun compile_source/1, Sources),

    %% To use in end_per_testcase
    Path = code:get_path(),
    {ok,Cwd} = file:get_cwd(),

    [{copy_dir, CopyDir}, {cwd,Cwd}, {path,Path} | Config].

compile_source(File) ->
    %% The compiler will no longer create a Beam file
    %% with a module name that does not match the output
    %% file, so we must compile to a binary and write
    %% the output file ourselves.
    U = filename:dirname(filename:dirname(File)),
    Base = filename:rootname(filename:basename(File)),
    OutFile = filename:join([U,"ebin",Base++".beam"]),
    OutFileTemp = OutFile ++ "#",
    {ok,_,Code} = compile:file(File, [binary]),
    ok = file:write_file(OutFileTemp, Code),
    file:rename(OutFileTemp, OutFile).

end_per_suite(Conf) when is_list(Conf) ->
    %% Nothing.
    Conf.

init_per_testcase(link_tar, Config) ->
    case os:type() of
	{unix, _} -> init_per_testcase(dummy, Config);
	{win32, _} -> {skip, "Skip on windows"}
    end;
init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    case {?config(path,Config),?config(cwd,Config)} of
	{undefined,undefined} ->
	    ok;
	{Path,Cwd} ->
	    true = code:set_path(Path),
	    ok = file:set_cwd(Cwd)
    end,
    ok.



%% Usage:
%%    systools:make_script("RelName")
%%                        Make a boot file from RelName.rel.
%%                        Generates RelName.{script,boot}
%%    systools:make_tar("RelName")
%%                        Make a release package from RelName.rel.
%%                        Generates RelName.tar,Z
%%    systools:script2boot(File)
%%                        File.script -> File.boot
%%    systools:make_relup("Target", ["UpFromRel"...], ["DownToRel"...], Opts)
%%			  Gather all appup scripts to the relup file
%%


%% make_script
%%
script_options(suite) -> [];
script_options(doc) ->
    ["Check illegal script options."];
script_options(Config) when is_list(Config) ->
    ?line {'EXIT',{{badarg,[{path,["Path",12,"Another"]}]}, _}} =
       (catch systools:make_script("release", [{path,["Path",12,"Another"]}])),
    ?line {'EXIT',{{badarg,[sillent]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path","Another"]},sillent])),
    ?line {'EXIT',{{badarg,[locall]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path","Another"]},locall])),
    ?line {'EXIT',{{badarg,[src_testsxx]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path"]},src_testsxx])),
    ?line {'EXIT',{{badarg,[{variables, {"TEST", "/home/lib"}}]}, _}} =
	(catch systools:make_script("release",
				    [{variables, {"TEST", "/home/lib"}}])),
    ?line {'EXIT',{{badarg,[{variables, [{a, b}, {"a", "b"}]}]}, _}} =
	(catch systools:make_script("release",
				    [{variables, [{a, b}, {"a", "b"}]}])),
    ?line {'EXIT',{{badarg,[exreff]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path","Another"]},exreff])),
    ?line {'EXIT',{{badarg,[{exref,["appl"]}]}, _}} =
	(catch systools:make_script("release", [{exref,["appl"]}])),
    ?line {'EXIT',{{badarg,[{machine, "appl"}]}, _}} =
	(catch systools:make_script("release", [{machine,"appl"}])),
    ok.


%% make_script
%%
normal_script(suite) -> [];
normal_script(doc) ->
    ["Check that make_script handles normal case."];
normal_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line PSAVE = code:get_path(),		% Save path

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P1 = fname([LibDir, 'db-2.1', ebin]),
    ?line P2 = fname([LibDir, 'fe-3.1', ebin]),

    ?line true = code:add_patha(P1),
    ?line true = code:add_patha(P2),

    ?line ok = file:set_cwd(LatestDir),

    ?line ok = systools:make_script(filename:basename(LatestName)),
    ?line {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Check the same but w. silent flag
    ?line {ok, _, []} = systools:make_script(LatestName, [silent]),

    %% Use the local option
    ?line ok = systools:make_script(LatestName, [local]),
    ?line ok = check_script_path(LatestName),

    %% use the path option
    ?line code:set_path(PSAVE),			% Restore path
    %% Mess up std path:
    ?line true = code:add_patha(fname([LibDir, 'db-1.0', ebin])),
    ?line true = code:add_patha(fname([LibDir, 'fe-2.1', ebin])),

    ?line error = systools:make_script(LatestName),	%should fail
    ?line ok = systools:make_script(LatestName,[{path, [P1, P2]}]),

    ?line ok = file:set_cwd(OldDir),
    ?line code:set_path(PSAVE),			% Restore path
    ok.


%% make_script
%%
no_mod_vsn_script(suite) -> [];
no_mod_vsn_script(doc) ->
    ["Check that make_script handles normal case.",
     "Modules specified without version in .app file (db-3.1)."
     "Note that this is now the normal way - i.e. systools now "
     "ignores the module versions in the .app file."];
no_mod_vsn_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line PSAVE = code:get_path(),		% Save path

    ?line {LatestDir, LatestName} = create_script(latest_no_mod_vsn,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P1 = fname([LibDir, 'db-3.1', ebin]),
    ?line P2 = fname([LibDir, 'fe-3.1', ebin]),

    ?line true = code:add_patha(P1),
    ?line true = code:add_patha(P2),

    ?line ok = file:set_cwd(LatestDir),

    ?line ok = systools:make_script(filename:basename(LatestName)),
    ?line {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Check the same but w. silent flag
    ?line {ok, _, []} = systools:make_script(LatestName, [silent]),

    %% Use the local option
    ?line ok = systools:make_script(LatestName, [local]),
    ?line ok = check_script_path(LatestName),

    %% use the path option
    ?line code:set_path(PSAVE),			% Restore path
    %% Mess up std path:
    ?line true = code:add_patha(fname([LibDir, 'db-1.0', ebin])),
    ?line true = code:add_patha(fname([LibDir, 'fe-2.1', ebin])),

    ?line error = systools:make_script(LatestName),	%should fail
    ?line ok = systools:make_script(LatestName,
				    [{path, [P1, P2]}]),

    ?line ok = file:set_cwd(OldDir),
    ?line code:set_path(PSAVE),			% Restore path
    ok.


%% make_script
%%
wildcard_script(suite) -> [];
wildcard_script(doc) ->
    ["Check that make_script handles wildcards in path."];
wildcard_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line WildDir = fname([LibDir, '*', ebin]),

    ?line ok = file:set_cwd(LatestDir),

    ?line error = systools:make_script(filename:basename(LatestName)),

    ?line ok = systools:make_script(LatestName,
				    [{path, [WildDir]}]),

    ?line {ok, _} = read_script_file(LatestName),	% Check readabillity

    ?line ok = file:set_cwd(OldDir),
    ok.


%% make_script
%%
variable_script(suite) -> [];
variable_script(doc) ->
    ["Add own installation dependent variable in script."];
variable_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line ok = systools:make_script(LatestName,
				    [{path, P},
				     {variables, [{"TEST", LibDir}]}]),

    %% Check variables
    ?line ok = check_var_script_file([fname(['$TEST', 'db-2.1', ebin]),
				      fname(['$TEST', 'fe-3.1', ebin])],
				     P,
				     LatestName),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% make_script
%%
abnormal_script(suite) -> [];
abnormal_script(doc) ->
    ["Abnormal cases."];
abnormal_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),

    ?line ok = file:set_cwd(LatestDir),
    ?line LibDir = fname([DataDir, d_bad_app_vsn, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    %% Check wrong app vsn
    ?line error = systools:make_script(LatestName, [{path, P}]),
    ?line {error,
	   systools_make,
	   [{error_reading, {db, {no_valid_version,
				  {{"should be","2.1"},
				   {"found file", _, "2.0"}}}}}]} =
	systools:make_script(LatestName, [silent, {path, P}]),

    ?line ok = file:set_cwd(OldDir),
    ok.


%% make_script
%%
src_tests_script(suite) -> [];
src_tests_script(doc) ->
    ["Do not check date of object file or that source code can be found."];
src_tests_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line PSAVE = code:get_path(),		% Save path

    ?line {LatestDir, LatestName} = create_script(latest,Config),
    ?line BootFile = LatestName ++ ".boot",

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_missing_src, lib]),
    ?line P1 = fname([LibDir, 'db-2.1', ebin]),
    ?line P2 = fname([LibDir, 'fe-3.1', ebin]),
    N = [P1, P2],

    ?line ok = file:set_cwd(LatestDir),

    %% Manipulate the modification date of a beam file so it seems
    %% older than its .erl file
    ?line Erl = filename:join([P1,"..","src","db1.erl"]),
    ?line {ok, FileInfo=#file_info{mtime={{Y,M,D},T}}} = file:read_file_info(Erl),
    ?line Beam = filename:join(P1,"db1.beam"),
    ?line ok=file:write_file_info(Beam, FileInfo#file_info{mtime={{Y-1,M,D},T}}),

    %% Remove a .erl file
    ?line Erl2 = filename:join([P1,"..","src","db2.erl"]),
    ?line file:delete(Erl2),

    %% Then make script

    %% .boot file should not exist
    ?line ok = file:delete(BootFile),
    ?line false = filelib:is_regular(BootFile),
    %% With warnings_as_errors and src_tests option, an error should be issued
    ?line error =
	systools:make_script(LatestName, [silent, {path, N}, src_tests,
					  warnings_as_errors]),
    ?line error =
	systools:make_script(LatestName, [{path, N}, src_tests,
					  warnings_as_errors]),

    %% due to warnings_as_errors .boot file should still not exist
    ?line false = filelib:is_regular(BootFile),

    %% Two warnings should be issued when src_tests is given
    %% 1. old object code for db1.beam
    %% 2. missing source code for db2.beam
    ?line {ok, _, [{warning,{obj_out_of_date,_}},
		   {warning,{source_not_found,_}}]} =
	systools:make_script(LatestName, [silent, {path, N}, src_tests]),

    %% .boot file should exist now
    ?line true = filelib:is_regular(BootFile),

    %% Without the src_tests option, no warning should be issued
    ?line {ok, _, []} =
	systools:make_script(LatestName, [silent, {path, N}]),

    %% Check that the old no_module_tests option (from the time when
    %% it was default to do the src_test) is ignored
    ?line {ok, _, [{warning,{obj_out_of_date,_}},
		   {warning,{source_not_found,_}}]} =
	systools:make_script(LatestName, [silent,
					  {path, N},
					  no_module_tests,
					  src_tests]),

    ?line ok = file:set_cwd(OldDir),
    ?line code:set_path(PSAVE),
    ok.

%% make_script
%%
warn_shadow_script(suite) -> [];
warn_shadow_script(doc) ->
    ["Check that jam file out of date warning doesn't",
     "shadow bad module version error."];
warn_shadow_script(Config) when is_list(Config) ->
    %% This test has been removed since the 'vsn' attribute is
    %% not used any more, starting with R6. No warning
    %% 'obj_out_of_date' seemed to be generated.
    true.


%% make_script
%%
crazy_script(suite) -> [];
crazy_script(doc) ->
    ["Do the crazy cases."];
crazy_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest, Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    %% Run with bad path
    ?line error = systools:make_script(LatestName),
    ?line {error, _, [{error_reading, _}, {error_reading, _}]} =
	systools:make_script(LatestName, [silent]),

    %% Run with .rel file lacking kernel
    ?line {LatestDir2, LatestName2} = create_script(latest_nokernel, Config),
    ?line ok = file:set_cwd(LatestDir2),

    ?line error = systools:make_script(LatestName2),
    ?line {error, _, {missing_mandatory_app,[kernel,stdlib]}} =
	systools:make_script(LatestName2, [silent,{path,P}]),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% make_script
%%
included_script(suite) -> [];
included_script(doc) ->
    ["Check that make_script handles generation of script",
     "for applications with included applications."];
included_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line {LatestDir, LatestName} = create_include_files(inc1, Config),
    ?line ok = file:set_cwd(LatestDir),
    ?line ok = systools:make_script(LatestName),
    ?line ok = check_include_script(LatestName,
				    [t1, t2, t3, t5, t4, t6],
				    [t1, t3, t6]),
    ?line ok = file:set_cwd(OldDir),
    ok.

%% make_script
%%
included_override_script(suite) -> [];
included_override_script(doc) ->
    ["Check that make_script handles generation of script",
     "for applications with included applications which are override by",
     "the .rel file."];
included_override_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line {LatestDir, LatestName} = create_include_files(inc2, Config),
    ?line ok = file:set_cwd(LatestDir),
    ?line ok = systools:make_script(LatestName),
    ?line ok = check_include_script(LatestName,
				    [t1, t2, t3, t4, t6, t5],
				    [t1, t3, t6, t5]),

    ?line {_, LatestName1} = create_include_files(inc3, Config),
    ?line ok = systools:make_script(LatestName1),
    ?line ok = check_include_script(LatestName1,
				    [t3, t5, t4, t6, t1, t2],
				    [t3, t6, t1, t2]),

    ?line {_, LatestName2} = create_include_files(inc4, Config),
    ?line ok = systools:make_script(LatestName2),
    ?line ok = check_include_script(LatestName2,
				    [t3, t4, t6, t5, t1, t2],
				    [t3, t6, t5, t1, t2]),

    ?line {_, LatestName3} = create_include_files(inc5, Config),
    ?line ok = systools:make_script(LatestName3),
    ?line ok = check_include_script(LatestName3,
				    [t3, t4, t6, t1, t2],
				    [t3, t6, t1, t2]),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% make_script
%%
included_fail_script(suite) -> [];
included_fail_script(doc) ->
    ["Check that make_script handles errors then generating",
     "script with included applications."];
included_fail_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line {LatestDir, LatestName} = create_include_files(inc6, Config),
    ?line ok = file:set_cwd(LatestDir),
    ?line {error, _, {undefined_applications,[t2]}} =
	systools:make_script(LatestName, [silent]),

    ?line {_, LatestName1} = create_include_files(inc7, Config),
    ?line {error, _, {duplicate_include,[{{t5,t7,_,_},{t5,t6,_,_}}]}} =
	systools:make_script(LatestName1, [silent]),

    ?line {_, LatestName3} = create_include_files(inc9, Config),
    ?line {error, _, {circular_dependencies,[{t10,_},{t8,_}]}} =
	systools:make_script(LatestName3, [silent]),

    ?line {_, LatestName4} = create_include_files(inc10, Config),
    ?line {error, _, [{error_reading,{t9,{override_include,[t7]}}}]} =
	systools:make_script(LatestName4, [silent]),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% make_script
%%
included_bug_script(suite) -> [];
included_bug_script(doc) ->
    ["Check that make_script handles generation of script",
     "with difficult dependency for included applications."];
included_bug_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line {LatestDir, LatestName} = create_include_files(inc11, Config),
    ?line ok = file:set_cwd(LatestDir),
    ?line ok = systools:make_script(LatestName),
    ?line ok = check_include_script(LatestName,
				    [t13, t11, t12],
				    [t11, t12]),
    ?line ok = file:set_cwd(OldDir),
    ok.


%% make_script
%%
otp_3065(suite) -> [];
otp_3065(doc) ->
    ["Circular dependencies in systools:make_script()."];
otp_3065(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line {LatestDir, LatestName} = create_include_files(otp_3065, Config),
    ?line ok = file:set_cwd(LatestDir),
    ?line ok = systools:make_script(LatestName),
    ?line ok = check_include_script(LatestName,
				    [aa12, chAts, chTraffic],
				    [chTraffic]),
    ?line ok = file:set_cwd(OldDir),
    ok.


%% make_script
%%
exref_script(suite) -> [];
exref_script(doc) ->
    ["Check that make_script exref option works."];
exref_script(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line PSAVE = code:get_path(),		% Save path

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line {ok, _, _} = systools:make_script(LatestName, [{path,P}, silent]),

    %% Complete exref
    ?line {ok, _, W1} = 
	systools:make_script(LatestName, [exref, {path,P}, silent]),
    ?line check_exref_warnings(with_db1, W1),
    ?line {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Only exref the db application.
    ?line {ok, _, W2} =
	systools:make_script(LatestName, [{exref,[db]}, {path,P}, silent]),
    ?line check_exref_warnings(with_db1, W2),
    ?line {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Only exref the fe application.
    ?line {ok, _, W3} =
	systools:make_script(LatestName, [{exref,[fe]}, {path,P}, silent]),
    ?line check_exref_warnings(without_db1, W3),
    ?line {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% exref the db and stdlib applications.
    ?line {ok, _, W4} =
	systools:make_script(LatestName, [{exref,[db,stdlib]}, {path,P}, silent]),
    ?line check_exref_warnings(with_db1, W4),
    ?line {ok, _} = read_script_file(LatestName),	% Check readabillity
    ?line ok = file:set_cwd(OldDir),
    ?line code:set_path(PSAVE),			% Restore path
    ok.

check_exref_warnings(with_db1, W) ->
    case get_exref(undef, W) of
	{ok, [{db2,non_existing_func,0},
	      {fe2,non_existing_func,0},
	      {lists,non_existing_func,1}]} ->
	    ok;
	{ok, L} ->
	    test_server:fail({exref_warning_undef, L});
	_E ->
	    test_server:fail({bad_undef,_E})
    end;
check_exref_warnings(without_db1, W) ->
    case get_exref(undef, W) of
	false ->
	    ok;
	{ok, L} ->
	    test_server:fail({exref_warning_undef, L})
    end.

get_exref(undef, W)   -> filter(no_hipe(get_exref1(exref_undef, W))).

filter(false) ->
    false;
filter({ok, W}) ->
    {ok, filter(W)};
filter(L) ->
    lists:filter(fun%({hipe_consttab,_,_}) -> false;
		    ({int,_,_}) -> false;
		    ({i,_,_}) -> false;
		    ({crypto,_,_}) -> false;
		    (_) -> true
		 end,
		 L).

get_exref1(T, [{warning, {T, Value}}|_]) -> {ok, Value};
get_exref1(T, [_|W])                     -> get_exref1(T, W);
get_exref1(_, [])                        -> false.

no_hipe(false) ->
    false;
no_hipe({ok, Value}) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    Hipe = "hipe",
	    Fun = fun({M,_,_}) -> not lists:prefix(Hipe, atom_to_list(M)) end,
	    NewValue = lists:filter(Fun, Value),
	    {ok, NewValue};
	_Arch ->
	    {ok, Value}
    end.

%% tar_options
%%
tar_options(suite) -> [];
tar_options(doc) ->
    ["Check illegal tar options."];
tar_options(Config) when is_list(Config) ->
    ?line {'EXIT',{{badarg,[{path,["Path",12,"Another"]}]}, _}} =
	(catch systools:make_tar("release", [{path,["Path",12,"Another"]}])),
    ?line {'EXIT',{{badarg,[sillent]}, _}} =
	(catch systools:make_tar("release",
				 [{path,["Path","Another"]},sillent])),
    ?line {'EXIT',{{badarg,[{dirs,["dirs"]}]}, _}} =
	(catch systools:make_tar("release", [{dirs, ["dirs"]}])),
    ?line {'EXIT',{{badarg,[{erts, illegal}]}, _}} =
	(catch systools:make_tar("release", [{erts, illegal}])),
    ?line {'EXIT',{{badarg,[src_testsxx]}, _}} =
	(catch systools:make_tar("release",
				 [{path,["Path"]},src_testsxx])),
    ?line {'EXIT',{{badarg,[{variables, [{a, b}, {"a", "b"}]}]}, _}} =
	(catch systools:make_tar("release",
				 [{variables, [{a, b}, {"a", "b"}]}])),
    ?line {'EXIT',{{badarg,[{var_tar, illegal}]}, _}} =
	(catch systools:make_tar("release", [{var_tar, illegal}])),
    ?line {'EXIT',{{badarg,[exreff]}, _}} =
	(catch systools:make_tar("release",
				 [{path,["Path","Another"]},exreff])),
    ?line {'EXIT',{{badarg,[{exref,["appl"]}]}, _}} =
	(catch systools:make_tar("release", [{exref,["appl"]}])),
    ?line {'EXIT',{{badarg,[{machine, "appl"}]}, _}} =
	(catch systools:make_tar("release", [{machine,"appl"}])),
    ok.


%% normal_tar
%%
normal_tar(suite) -> [];
normal_tar(doc) ->
    ["Check normal case"];
normal_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line {ok, _, _} = systools:make_script(LatestName, [silent, {path, P}]),
    ?line ok = systools:make_tar(LatestName, [{path, P}]),
    ?line ok = check_tar(fname([lib,'db-2.1',ebin,'db.app']), LatestName),
    ?line {ok, _, _} = systools:make_tar(LatestName, [{path, P}, silent]),
    ?line ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% no_mod_vsn_tar
%%
no_mod_vsn_tar(suite) -> [];
no_mod_vsn_tar(doc) ->
    ["Check normal case",
     "Modules specified without version in .app file (db-3.1)."
     "Note that this is now the normal way - i.e. systools now "
     "ignores the module versions in the .app file."];
no_mod_vsn_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest_no_mod_vsn,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-3.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line {ok, _, _} = systools:make_script(LatestName, [silent, {path, P}]),
    ?line ok = systools:make_tar(LatestName, [{path, P}]),
    ?line ok = check_tar(fname([lib,'db-3.1',ebin,'db.app']), LatestName),
    ?line {ok, _, _} = systools:make_tar(LatestName, [{path, P}, silent]),
    ?line ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% variable_tar
%%
variable_tar(suite) -> [];
variable_tar(doc) ->
    ["Use variable and create separate tar (included in generated tar)."];
variable_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line {ok, _, _} = systools:make_script(LatestName,
					    [silent,
					     {path, P},
					     {variables,[{"TEST", LibDir}]}]),

    ?line ok = systools:make_tar(LatestName, [{path, P},
					      {variables,[{"TEST", LibDir}]}]),
    ?line ok = check_var_tar("TEST", LatestName),

    ?line {ok, _, _} = systools:make_tar(LatestName,
					 [{path, P}, silent,
					  {variables,[{"TEST", LibDir}]}]),
    ?line ok = check_var_tar("TEST", LatestName),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% link_tar
%%
link_tar(suite) -> [];
link_tar(doc) ->
    ["Check that symlinks in applications are handled correctly"];
link_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_links, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    %% Make some links
    ?line Db1Erl = fname(['db-2.1',src,'db1.erl']),
    ?line NormalDb1Erl = fname([DataDir,d_normal,lib,Db1Erl]),
    ?line LinkDb1Erl = fname([LibDir, Db1Erl]),
    ?line ok = file:make_symlink(NormalDb1Erl, LinkDb1Erl),
    ?line Db1Beam = fname(['db-2.1',ebin,'db1.beam']),
    ?line NormalDb1Beam = fname([DataDir,d_normal,lib,Db1Beam]),
    ?line LinkDb1Beam = fname([LibDir, Db1Beam]),
    ?line ok = file:make_symlink(NormalDb1Beam, LinkDb1Beam),
    ?line FeApp = fname(['fe-3.1',ebin,'fe.app']),
    ?line NormalFeApp = fname([DataDir,d_normal,lib,FeApp]),
    ?line LinkFeApp = fname([LibDir, FeApp]),
    ?line ok = file:make_symlink(NormalFeApp, LinkFeApp),
    
    %% Create the tar and check that the linked files are included as
    %% regular files
    ?line ok = file:set_cwd(LatestDir),

    ?line {ok,_,[]} = systools:make_script(LatestName, [{path, P},silent]),

    ?line {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent]),
    ?line ok = check_tar_regular(?privdir,
				 [fname([lib,FeApp]),
				  fname([lib,Db1Beam])],
				 LatestName),
    
    ?line {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent,
						       {dirs, [src]}]),
    ?line ok = check_tar_regular(?privdir,
				 [fname([lib,FeApp]),
				  fname([lib,Db1Beam]),
				  fname([lib,Db1Erl])],
				 LatestName),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% src_tests_tar
%%
src_tests_tar(suite) -> [];
src_tests_tar(doc) ->
    ["Do not check date of object file or that source code can be found."];
src_tests_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_missing_src, lib]),
    ?line P1 = fname([LibDir, 'db-2.1', ebin]),
    ?line P2 = fname([LibDir, 'fe-3.1', ebin]),
    P = [P1, P2],

    ?line ok = file:set_cwd(LatestDir),

    %% Manipulate the modification date of a beam file so it seems
    %% older than the .erl file
    Erl = filename:join([P1,"..","src","db1.erl"]),
    {ok, FileInfo=#file_info{mtime={{Y,M,D},T}}} = file:read_file_info(Erl),
    Beam = filename:join(P1,"db1.beam"),
    ok = file:write_file_info(Beam, FileInfo#file_info{mtime={{Y-1,M,D},T}}),

    %% Remove a .erl file
    ?line Erl2 = filename:join([P1,"..","src","db2.erl"]),
    ?line file:delete(Erl2),

    ?line ok = systools:make_script(LatestName, [{path, P}]),

    %% Then make tar - two warnings should be issued when
    %% src_tests is given
    %% 1. old object code for db1.beam
    %% 2. missing source code for db2.beam
    ?line {ok, _, [{warning,{obj_out_of_date,_}},
    		   {warning,{source_not_found,_}}]} =
	systools:make_tar(LatestName, [{path, P}, silent,
				       {dirs, [src]},
				       src_tests]),
    ?line ok = check_tar(fname([lib,'db-2.1',src,'db1.erl']), LatestName),

    %% Without the src_tests option, no warning should be issued
    ?line {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent,
						       {dirs, [src]}]),
    ?line ok = check_tar(fname([lib,'db-2.1',src,'db1.erl']), LatestName),

    %% Check that the old no_module_tests option (from the time when
    %% it was default to do the src_test) is ignored
    ?line {ok, _, [{warning,{obj_out_of_date,_}},
    		   {warning,{source_not_found,_}}]} =
	systools:make_tar(LatestName, [{path, P}, silent,
				       {dirs, [src]},
				       no_module_tests,
				       src_tests]),
    ?line ok = check_tar(fname([lib,'db-2.1',src,'db1.erl']), LatestName),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% shadow_tar
%%
shadow_tar(suite) -> [];
shadow_tar(doc) ->
    ["Check that jam file out of date warning doesn't",
     "shadow bad module version error."];
shadow_tar(Config) when is_list(Config) ->
    % This test has been commented out since the 'vsn' attribute is not used
    % any more, starting with R6. No warning 'obj_out_of_date' seemed to be
    % generated.
    true;
shadow_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line PSAVE = code:get_path(),		% Save path

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, 'd_bad_mod+warn', lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line {error, _, _} = systools:make_tar(LatestName, [{path, P}, silent]),
    ?line {error, _, _} = systools:make_tar(LatestName, [{path, P}, silent,
							 {dirs, [src]}]),
    ?line ok = file:set_cwd(OldDir),
    ?line code:set_path(PSAVE),
    ok.


%% var_tar
%%
var_tar(suite) -> [];
var_tar(doc) ->
    ["Check that make_tar handles generation and placement of tar",
     "files for variables outside the main tar file.",
     "Test the {var_tar, include | ownfile | omit} option."];
var_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),
    ?line PSAVE = code:get_path(),		% Save path

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line {ok, _, _} = systools:make_script(LatestName,
					    [silent,
					     {path, P},
					     {variables,[{"TEST", LibDir}]}]),

    ?line ok = systools:make_tar(LatestName, [{path, P},
					      {var_tar, ownfile},
					      {variables,[{"TEST", LibDir}]}]),

    ?line true = exists_tar_file("TEST"), %% Also removes the file !
    ?line {error, {not_generated, _}} = check_var_tar("TEST", LatestName),

    ?line ok = systools:make_tar(LatestName, [{path, P},
					      {var_tar, omit},
					      {variables,[{"TEST", LibDir}]}]),

    ?line {error, {not_generated, _}} = check_var_tar("TEST", LatestName),
    ?line false = exists_tar_file("TEST"),

    ?line ok = systools:make_tar(LatestName, [{path, P},
					      {var_tar, include},
					      {variables,[{"TEST", LibDir}]}]),

    ?line ok = check_var_tar("TEST", LatestName),
    ?line false = exists_tar_file("TEST"),

    ?line ok = file:set_cwd(OldDir),
    ?line code:set_path(PSAVE),
    ok.


%% exref_tar
%%
exref_tar(suite) -> [];
exref_tar(doc) ->
    ["Check exref option."];
exref_tar(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'fe-3.1', ebin])],

    ?line ok = file:set_cwd(LatestDir),

    ?line {ok, _, _} = systools:make_script(LatestName, [silent, {path, P}]),

    %% Complete exref
    ?line {ok, _, W1} =
	systools:make_tar(LatestName, [exref, {path, P}, silent]),
    ?line check_exref_warnings(with_db1, W1),
    ?line ok = check_tar(fname([lib,'db-2.1',ebin,'db.app']), LatestName),

    %% Only exref the db application.
    ?line {ok, _, W2} =
	systools:make_tar(LatestName, [{exref, [db]}, {path, P}, silent]),
    ?line check_exref_warnings(with_db1, W2),
    ?line ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    %% Only exref the fe application.
    ?line {ok, _, W3} =
	systools:make_tar(LatestName, [{exref, [fe]}, {path, P}, silent]),
    ?line check_exref_warnings(without_db1, W3),
    ?line ok = check_tar(fname([lib,'db-2.1',ebin,'db.app']), LatestName),

    %% exref the db and stdlib applications.
    ?line {ok, _, W4} =
	systools:make_tar(LatestName, [{exref, [db, stdlib]},
				       {path, P}, silent]),
    ?line check_exref_warnings(with_db1, W4),
    ?line ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    ?line ok = file:set_cwd(OldDir),
    ok.



%% otp_9507
%%
otp_9507(suite) -> [];
otp_9507(doc) ->
    ["make_tar failed when path given as just 'ebin'."];
otp_9507(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest_small,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line FeDir = fname([LibDir, 'fe-3.1']),

    ?line ok = file:set_cwd(FeDir),

    RelName = fname([LatestDir,LatestName]),

    ?line P1 = ["./ebin",
	       fname([DataDir, lib, kernel, ebin]),
	       fname([DataDir, lib, stdlib, ebin])],
    ?line {ok, _, _} = systools:make_script(RelName, [silent, {path, P1}]),
    ?line ok = systools:make_tar(RelName, [{path, P1}]),
    ?line Content1 = tar_contents(RelName),

    ?line P2 = ["ebin",
	       fname([DataDir, lib, kernel, ebin]),
	       fname([DataDir, lib, stdlib, ebin])],

    %% Tickets solves the following line - it used to fail with
    %% {function_clause,[{filename,join,[[]]},...}
    ?line ok = systools:make_tar(RelName, [{path, P2}]),
    ?line Content2 = tar_contents(RelName),
    true = (Content1 == Content2),

    ?line ok = file:set_cwd(OldDir),

    ok.


%% The relup stuff.
%%
%%


%% make_relup
%%
normal_relup(suite) -> [];
normal_relup(doc) ->
    ["Check normal case"];
normal_relup(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir,LatestName}   = create_script(latest0,Config),
    ?line {_LatestDir1,LatestName1} = create_script(latest1,Config),
    ?line {_LatestDir2,LatestName2} = create_script(latest2,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = [fname([DataDir, d_normal, lib])],
    ?line P = [fname([LibDir, '*', ebin]),
	       fname([DataDir, lib, kernel, ebin]),
	       fname([DataDir, lib, stdlib, ebin])],

    ?line ok = file:set_cwd(LatestDir),

    %% OTP-2561: Check that the option 'restart_emulator' generates a
    %% "restart_new_emulator" instruction.
    ?line {ok, _ , _, []} =
         systools:make_relup(LatestName, [LatestName1], [LatestName1],
			     [{path, P},restart_emulator,silent]),
    ?line ok = check_relup([{db, "2.1"}], [{db, "1.0"}]),
    ?line ok = check_restart_emulator(),

    %% This is the ultra normal case
    ?line ok = systools:make_relup(LatestName, [LatestName1], [LatestName1],
				   [{path, P}]),
    ?line ok = check_relup([{db, "2.1"}], [{db, "1.0"}]),
    ?line {ok, _, _, []} =
	systools:make_relup(LatestName, [LatestName1], [LatestName1],
			    [{path, P}, silent]),
    ?line ok = check_relup([{db, "2.1"}], [{db, "1.0"}]),

    %% file should not be written if warnings_as_errors is enabled.
    %% delete before running tests.
    ?line ok = file:delete("relup"),

    %% Check that warnings are treated as errors
    ?line error =
	systools:make_relup(LatestName, [LatestName2], [LatestName1],
			    [{path, P}, warnings_as_errors]),
    ?line error =
	systools:make_relup(LatestName, [LatestName2], [LatestName1],
			    [{path, P}, silent, warnings_as_errors]),

    %% relup file should not exist
    ?line false = filelib:is_regular("relup"),

    %% Check that warnings get through
    ?line ok = systools:make_relup(LatestName, [LatestName2], [LatestName1],
				   [{path, P}]),
    ?line ok = check_relup([{fe, "3.1"}, {db, "2.1"}], [{db, "1.0"}]),
    ?line {ok, _, _, [{erts_vsn_changed, _}]} =
	systools:make_relup(LatestName, [LatestName2], [LatestName1],
			    [{path, P}, silent]),
    ?line ok = check_relup([{fe, "3.1"}, {db, "2.1"}], [{db, "1.0"}]),

    %% relup file should exist now
    ?line true = filelib:is_regular("relup"),

    ?line ok = file:set_cwd(OldDir),
    ok.


%% This test fails if wrong version numbers are seen in the relup file
%% or if any application is missing. This was triggered by OTP-1360.
check_relup(UpVsnL, DnVsnL) ->
    {ok, [{_V1, [{_, _, Up}], [{_, _, Dn}]}]} = file:consult(relup),
    [] = foldl(fun(X, Acc) ->
		       true = lists:member(X, Acc),
		       lists:delete(X, Acc) end,
	       UpVsnL,
	       [{App, Vsn} || {load_object_code,{App,Vsn,_}} <- Up]),
    [] = foldl(fun(X, Acc) ->
		       true = lists:member(X, Acc),
		       lists:delete(X, Acc) end,
	       DnVsnL,
	       [{App, Vsn} || {load_object_code,{App,Vsn,_}} <- Dn]),
    ok.

check_restart_emulator() ->
    {ok, [{_V1, [{_, _, Up}], [{_, _, Dn}]}]} = file:consult(relup),
    restart_new_emulator = lists:last(Up),
    restart_new_emulator = lists:last(Dn),
    ok.

%% make_relup
%%
no_appup_relup(suite) -> [];
no_appup_relup(doc) ->
    ["Check that appup files may be missing, but only if we don't need them."];
no_appup_relup(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir,LatestName}   = create_script(latest_small,Config),
    ?line {_LatestDir0,LatestName0} = create_script(latest_small0,Config),
    ?line {_LatestDir1,LatestName1} = create_script(latest_small1,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line P1 = [fname([DataDir, d_no_appup, lib, 'fe-3.1', ebin]),
		fname([DataDir, lib, kernel, ebin]),
		fname([DataDir, lib, stdlib, ebin])],

    ?line ok = file:set_cwd(LatestDir),

    %% Check that appup might be missing
    ?line ok =
	systools:make_relup(LatestName, [LatestName], [], [{path, P1}]),
    ?line {ok,_, _, []} =
	systools:make_relup(LatestName, [LatestName], [],
			    [silent, {path, P1}]),

    %% Check that appup might NOT be missing when we need it
    ?line error =
	systools:make_relup(LatestName, [LatestName0], [], [{path, P1}]),
    ?line {error,_,{file_problem, {_,{error,{open,_,_}}}}} =
	systools:make_relup(LatestName, [], [LatestName0],
			    [silent, {path, P1}]),

    %% Check that appups missing vsn traps
    ?line P2 = [fname([DataDir, d_no_appup, lib, 'fe-2.1', ebin]),
		fname([DataDir, lib, kernel, ebin]),
		fname([DataDir, lib, stdlib, ebin])],

    ?line error =
	systools:make_relup(LatestName0, [LatestName1], [], [{path, P2}]),
    ?line {error,_,{no_relup, _, _, _}} =
	systools:make_relup(LatestName0, [], [LatestName1],
			    [silent, {path, P2}]),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% make_relup
%%
bad_appup_relup(suite) -> [];
bad_appup_relup(doc) ->
    ["Check that badly written appup files are detected"];
bad_appup_relup(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir,LatestName}   = create_script(latest_small,Config),
    ?line {_LatestDir0,LatestName0} = create_script(latest_small0,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line N2 = [fname([DataDir, d_bad_appup, lib, 'fe-3.1', ebin]),
		fname([DataDir, lib, kernel, ebin]),
		fname([DataDir, lib, stdlib, ebin])],

    ?line ok = file:set_cwd(LatestDir),

    %% Check that bad appup is trapped
    ?line error =
	systools:make_relup(LatestName, [LatestName0], [], [{path, N2}]),
    ?line {error,_,{file_problem, {_, {error, {parse,_, _}}}}} =
	systools:make_relup(LatestName, [], [LatestName0],
			    [silent, {path, N2}]),

    ?line ok = file:set_cwd(OldDir),
    ok.

%% make_relup
%%
abnormal_relup(suite) -> [];
abnormal_relup(doc) ->
    ["Check some abnormal cases"];
abnormal_relup(Config) when is_list(Config) ->
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir,LatestName}   = create_script(latest0,Config),
    ?line {_LatestDir1,LatestName1} = create_script(latest1,Config),

    %% Check wrong app vsn
    ?line DataDir = filename:absname(?copydir),
    ?line P = [fname([DataDir, d_bad_app_vsn, lib, 'db-2.1', ebin]),
	       fname([DataDir, d_bad_app_vsn, lib, 'fe-3.1', ebin]),
	       fname([DataDir, lib, kernel, ebin]),
	       fname([DataDir, lib, stdlib, ebin])],
    
    ?line ok = file:set_cwd(LatestDir),

    ?line error = systools:make_relup(LatestName, [LatestName1], [LatestName1],
				      [{path, P}]),
    ?line R0 = systools:make_relup(LatestName, [LatestName1], [LatestName1],
			      [silent, {path, P}]),
    ?line {error,systools_make,
	     [{error_reading,{db,{no_valid_version,
				  {{"should be","2.1"},
				   {"found file", _, "2.0"}}}}}]} = R0,
    ?line ok = file:set_cwd(OldDir),
    ok.


%% Check that application start type is used in relup
app_start_type_relup(suite) ->
    [];
app_start_type_relup(doc) ->
    ["Release upgrade file with various application start types"];
app_start_type_relup(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line {Dir1,Name1} = create_script(latest_app_start_type1,Config),
    ?line {Dir2,Name2} = create_script(latest_app_start_type2,Config),
    ?line Release1 = filename:join(Dir1,Name1),
    ?line Release2 = filename:join(Dir2,Name2),

    ?line {ok, Release2Relup, systools_relup, []} = systools:make_relup(Release2, [Release1], [Release1], [{outdir, PrivDir}, silent]),
    ?line {"2", [{"1",[], UpInstructions}], [{"1",[], DownInstructions}]} = Release2Relup,
    %% ?t:format("Up: ~p",[UpInstructions]),
    %% ?t:format("Dn: ~p",[DownInstructions]),
    ?line [{load_object_code, {mnesia, _, _}},
           {load_object_code, {sasl, _, _}},
           {load_object_code, {webtool, _, _}},
           {load_object_code, {snmp, _, _}},
           {load_object_code, {xmerl, _, _}},
           point_of_no_return
           | UpInstructionsT] = UpInstructions,
    ?line true = lists:member({apply,{application,start,[mnesia,permanent]}}, UpInstructionsT),
    ?line true = lists:member({apply,{application,start,[sasl,transient]}}, UpInstructionsT),
    ?line true = lists:member({apply,{application,start,[webtool,temporary]}}, UpInstructionsT),
    ?line true = lists:member({apply,{application,load,[snmp]}}, UpInstructionsT),
    ?line false = lists:any(fun({apply,{application,_,[xmerl|_]}}) -> true; (_) -> false end, UpInstructionsT),
    ?line [point_of_no_return | DownInstructionsT] = DownInstructions,
    ?line true = lists:member({apply,{application,stop,[mnesia]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[sasl]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[webtool]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[snmp]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[xmerl]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[mnesia]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[sasl]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[webtool]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[snmp]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[xmerl]}}, DownInstructionsT),
    ok.


otp_6226(suite) ->
    [];
otp_6226(doc) ->
    ["{outdir,Dir} option for systools:make_script()"];
otp_6226(Config) when is_list(Config) ->
    PrivDir = ?privdir,
    ?line {ok, OldDir} = file:get_cwd(),

    ?line {LatestDir, LatestName} = create_script(latest0,Config),
    ?line {_LatestDir, LatestName1} = create_script(latest1,Config),

    ?line DataDir = filename:absname(?copydir),
    ?line LibDir = fname([DataDir, d_normal, lib]),
    ?line P = [fname([LibDir, 'db-2.1', ebin]),
	       fname([LibDir, 'db-1.0', ebin]),
	       fname([LibDir, 'fe-3.1', ebin]),
	       fname([DataDir, lib, kernel, ebin]),
	       fname([DataDir, lib, stdlib, ebin])],

    ?line ok = file:set_cwd(LatestDir),

 
    %% Create an outdir1 directory
    ?line ok = file:make_dir("outdir1"),

    %% ==== Now test systools:make_script ====
    %% a) badarg
    ?line {'EXIT', {{badarg,[{outdir,outdir1}]}, _}} =
	(catch systools:make_script(LatestName, [{outdir,outdir1},
						 {path,P},silent])),

    %% b) absolute path
    Outdir1 = filename:join(PrivDir, "outdir1"),
    ?line {ok,_,[]} = systools:make_script(LatestName, [{outdir,Outdir1},
							{path,P},silent]),
    ?line Script1 = filename:join(Outdir1, LatestName ++ ".script"),
    ?line Boot1 = filename:join(Outdir1, LatestName ++ ".boot"),
    ?line true = filelib:is_file(Script1),
    ?line true = filelib:is_file(Boot1),
    ?line ok = file:delete(Script1),
    ?line ok = file:delete(Boot1),

    %% c) relative path
    ?line {ok,_,[]} = systools:make_script(LatestName, [{outdir,"./outdir1"},
							{path,P},silent]),
    ?line true = filelib:is_file(Script1),
    ?line true = filelib:is_file(Boot1),
    ?line ok = file:delete(Script1),
    ?line ok = file:delete(Boot1),

    %% d) absolute but incorrect path
    ?line Outdir2 = filename:join(PrivDir, "outdir2"),
    ?line Script2 = filename:join(Outdir2, LatestName ++ ".script"),
    ?line {error,_,{open,Script2,_}} = 
	systools:make_script(LatestName, [{outdir,Outdir2},{path,P},silent]),

    %% e) relative but incorrect path
    ?line {error,_,{open,_,_}} = 
	systools:make_script(LatestName, [{outdir,"./outdir2"},{path,P},silent]),

    %% f) with .rel in another directory than cwd
    ?line ok = file:set_cwd(Outdir1),
    ?line {ok,_,[]} = systools:make_script(filename:join(PrivDir, LatestName),
					   [{outdir,"."},{path,P},silent]),
    ?line true = filelib:is_file(LatestName ++ ".script"),
    ?line true = filelib:is_file(LatestName ++ ".boot"),
    ?line ok = file:delete(LatestName ++ ".script"),
    ?line ok = file:delete(LatestName ++ ".boot"),
    ?line ok = file:set_cwd(LatestDir),

    %% ==== Now test systools:make_tar =====
    ?line {ok,_,[]} = systools:make_script(LatestName, [{path,P},silent]),
    %% a) badarg
    ?line {'EXIT', {{badarg, [{outdir,outdir1}]}, _}} =
    	(catch systools:make_tar(LatestName,[{outdir,outdir1},{path,P},silent])),

    %% b) absolute path
    ?line {ok,_,[]} = systools:make_tar(LatestName, [{outdir,Outdir1}, 
						     {path,P},silent]),
    ?line Tar1 = filename:join(Outdir1,LatestName++".tar.gz"),
    ?line true = filelib:is_file(Tar1),
    ?line ok = file:delete(Tar1),

    %% c) relative path
    ?line {ok,_,[]} = systools:make_tar(LatestName, [{outdir,"./outdir1"},
						     {path,P},silent]),
    ?line true = filelib:is_file(Tar1),
    ?line ok = file:delete(Tar1),

    %% d) absolute but incorrect path
    ?line Tar2 = filename:join(Outdir2,LatestName++".tar.gz"),
    ?line {error,_,{tar_error,{open,Tar2,{Tar2,enoent}}}} = 
	systools:make_tar(LatestName, [{outdir,Outdir2},{path,P},silent]),
    
    %% e) relative but incorrect path
    ?line {error,_,{tar_error,{open,_,_}}} = 
	   systools:make_tar(LatestName, [{outdir,"./outdir2"},{path,P},silent]),

    %% f) with .rel in another directory than cwd
    ?line ok = file:set_cwd(Outdir1),
    ?line {ok,_,[]} = systools:make_tar(filename:join(PrivDir, LatestName),
					[{outdir,"."},{path,P},silent]),
    ?line true = filelib:is_file(Tar1),
    ?line ok = file:set_cwd(LatestDir),

    %% ===== Now test systools:make_relup =====
    %% a) badarg
    ?line {'EXIT', {{badarg, [{outdir,outdir1}]}, _}} =
    	(catch systools:make_relup(LatestName,[LatestName1],[LatestName1],
    				   [{outdir,outdir1}, 
				    {path,P},silent])),

    %% b) absolute path
    Relup = filename:join(Outdir1, "relup"),
    ?line {ok,_,_,[]} = systools:make_relup(LatestName,[LatestName1],[LatestName1],
					    [{outdir,Outdir1},
					     {path,P},silent]),
    ?line true = filelib:is_file(Relup),
    ?line ok = file:delete(Relup),
    
    %% c) relative path
    ?line {ok,_,_,[]} = systools:make_relup(LatestName,[LatestName1],[LatestName1],
					    [{outdir,"./outdir1"},
					     {path,P},silent]),
    ?line true = filelib:is_file(Relup),
    ?line ok = file:delete(Relup),
    
    %% d) absolute but incorrect path
    ?line {error,_,{file_problem,{"relup",enoent}}} = 
	systools:make_relup(LatestName,[LatestName1],[LatestName1],
			    [{outdir,Outdir2},{path,P},silent]),
    
    %% e) relative but incorrect path
    ?line {error,_,{file_problem,{"relup",enoent}}} = 
	systools:make_relup(LatestName,[LatestName1],[LatestName1],
			    [{outdir,"./outdir2"},{path,P},silent]),

    %% f) with .rel in another directory than cwd
    %% -- not necessary to test since relup by default is placed in
    %%    cwd, not in the same directory as the .rel file --

    %% Change back to previous working directory
    ?line ok = file:set_cwd(OldDir),
    ok.


%%%%%%
%%%%%% Utilities
%%%%%%

check_script_path(RelName) ->
    {ok, [Conts]} = read_script_file(RelName),
    {script, {_, _}, ListOfThings} = Conts,
    case lists:keysearch(path, 1, ListOfThings) of
	{value, {path, [$$,$R,$O,$O,$T | _]}} -> %"$ROOT..."
	    false;
	_ -> ok
    end.

check_var_script_file(VarDirs, NoExistDirs, RelName) ->
    {ok, [Conts]} = read_script_file(RelName),
    {script, {_, _}, ListOfThings} = Conts,
    AllPaths = lists:append(lists:map(fun({path, P}) -> P;
					 (_)         -> []
				      end,
				  ListOfThings)),
    case lists:filter(fun(VarDir) -> lists:member(VarDir, AllPaths) end,
		      VarDirs) of
	VarDirs ->
	    ok;
	_ ->
	    test_server:fail("All variable dirs not in generated script")
    end,
    case lists:filter(fun(NoExistDir) -> lists:member(NoExistDir, AllPaths) end,
		      NoExistDirs) of
	[] ->
	    ok;
	_ ->
	    test_server:fail("Unexpected dirs in generated script")
    end.

check_include_script(RelName, ExpectedLoad, ExpectedStart) ->
    {ok, [Conts]} = read_script_file(RelName),
    {script, {_, _}, ListOfThings} = Conts,

    %% Check that the applications are loaded in given order !
    ActualLoad = 
	[App || {apply,{application,load,[{application,App,_}]}} <- ListOfThings,
		App=/=kernel,
		App=/=stdlib],
    
    if ActualLoad =:= ExpectedLoad -> ok;
       true -> test_server:fail({bad_load_order, ActualLoad, ExpectedLoad})
    end,

    %% Check that applications are started in given order !
    ActualStart =
	[App || {apply,{application,start_boot,[App|_]}} <- ListOfThings,
		App =/= kernel,
		App =/= stdlib],

    if ActualStart =:= ExpectedStart -> ok;
       true -> test_server:fail({bad_start_order, ActualStart,ExpectedStart})
    end,

    ok.

read_script_file(RelName) ->
    file:consult(RelName ++ ".script").

check_var_tar(Variable, RelName) ->
    Expected = tar_name(Variable),
    case check_tar(Expected,RelName) of
	ok ->
	    ok;
	{error, {erroneous_tar_file, _, missing, _}} ->
	    {error, {not_generated, Expected}}
    end.

exists_tar_file(Name) ->
    File = tar_name(Name),
    case filelib:is_regular(File) of
	true ->
	    ok = file:delete(File),
	    true;
	_ ->
	    false
    end.

%% Take a snap of the generated tar file and check if a certain
%% file is included.
%% This ensures at least that the tar file is generated.
check_tar(File, RelName) ->
    TarContents = tar_contents(RelName),
    case lists:member(File,TarContents) of
	true -> ok;
	_    -> {error, {erroneous_tar_file, tar_name(RelName), missing, File}}
    end.

%% Check that the given files exist in the tar file, and that they are
%% not symlinks
check_tar_regular(PrivDir, Files, RelName) ->
    TmpDir = fname(PrivDir,tmp),
    ok = file:make_dir(TmpDir),
    ok = erl_tar:extract(tar_name(RelName),
			 [{files,Files},{cwd,TmpDir},compressed]),
    R = lists:foldl(fun(File,Acc) ->
			    case file:read_link_info(fname(TmpDir,File)) of 
				{ok,#file_info{type=regular}} ->
				    Acc;
				{ok,#file_info{type=Other}} ->
				    [{File,Other}|Acc];
				_ ->
				    [{File,missing}|Acc]
			    end
		    end,
		    [],
		    Files),
    delete_tree(TmpDir),
    case R of
	[] ->
	    ok;
	NotThere ->
	    {error,{erroneous_tar_file,tar_name(RelName),NotThere}}
    end.
	    
delete_tree(Dir) ->
    case filelib:is_dir(Dir) of
	true ->
	    {ok,Files} = file:list_dir(Dir),
	    lists:foreach(fun(File) -> delete_tree(filename:join(Dir,File)) end, 
			  Files),
	    file:del_dir(Dir);
	false ->
	    ok = file:delete(Dir)
    end.

tar_contents(Name) ->
    {ok, Cont} = erl_tar:table(Name ++ ".tar.gz", [compressed]),
    Cont.

tar_name(Name) ->
    Name ++ ".tar.gz".

create_script(latest,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, latest),
    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 3\", \"LATEST\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"~s\"}, {stdlib, \"~s\"}, \n"
		    "  {db, \"2.1\"}, {fe, \"3.1\"}]}.\n",
		    [KernelVer,StdlibVer]),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest_no_mod_vsn,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, latest),
    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 3\", \"LATESTNOMOD\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"~s\"}, {stdlib, \"~s\"}, \n"
		    "  {db, \"3.1\"}, {fe, \"3.1\"}]}.\n",
		    [KernelVer,StdlibVer]),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest0,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, 'latest-1'),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 2\", \"LATEST0\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"1.0\"}, {stdlib, \"1.0\"}, \n"
		    "  {db, \"2.1\"}, {fe, \"3.1\"}]}.\n",
		    []),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest1,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, latest),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 2\", \"LATEST1\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"1.0\"}, {stdlib, \"1.0\"}, \n"
		    "  {db, \"1.0\"}, {fe, \"3.1\"}]}.\n",
		    []),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest2,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, 'latest-2'),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 1\", \"LATEST2\"}, \n"
		    " {erts, \"4.3\"}, \n"
		    " [{kernel, \"1.0\"}, {stdlib, \"1.0\"}, \n"
		    "  {db, \"1.0\"}, {fe, \"2.1\"}]}.\n",
		    []),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest_small,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, 'latest-small'),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 2\", \"LATEST_SMALL\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"1.0\"}, {stdlib, \"1.0\"}, \n"
		    "  {fe, \"3.1\"}]}.\n",
		    []),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest_small0,Config) ->		%Differs in fe vsn
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, 'latest-small0'),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 2\", \"LATEST_SMALL0\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"1.0\"}, {stdlib, \"1.0\"}, \n"
		    "  {fe, \"2.1\"}]}.\n",
		    []),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest_small1,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, 'latest-small1'),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 2\", \"LATEST_SMALL1\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{kernel, \"1.0\"}, {stdlib, \"1.0\"}, \n"
		    "  {fe, \"500.18.7\"}]}.\n",
		    []),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest_nokernel,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, 'latest-nokernel'),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line io:format(Fd,
		    "{release, {\"Test release 3\", \"LATEST_NOKERNEL\"}, \n"
		    " {erts, \"4.4\"}, \n"
		    " [{db, \"2.1\"}, {fe, \"3.1\"}]}.\n",
		    []),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest_app_start_type1,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, latest_app_start_type1),
    ?line ErtsVer = erlang:system_info(version),
    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line RelfileContent = 
	{release,{"Test release", "1"},
	 {erts,ErtsVer},
	 [{kernel,KernelVer},
	  {stdlib,StdlibVer}]},
    ?line io:format(Fd,"~p.~n",[RelfileContent]),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)};
create_script(latest_app_start_type2,Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, latest_app_start_type2),
    ?line ErtsVer = erlang:system_info(version),
    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    ?line OtherApps = [{mnesia,permanent},
		       {sasl,transient},
		       {webtool,temporary},
		       {snmp,load},
		       {xmerl,none}],
    ?line lists:foreach(fun({App,_}) -> application:load(App) end,
			OtherApps),
    ?line Loaded = application:loaded_applications(),
    ?line OtherAppsRel = 
	lists:map(fun({App,StartType}) -> 
			  {_,_,Ver} = lists:keyfind(App,1,Loaded),
			  {App,Ver,StartType}
		  end,
		  OtherApps),
    ?line {ok,Fd} = file:open(Name++".rel",write),
    ?line RelfileContent = 
	{release,{"Test release", "2"},
	 {erts,ErtsVer},
	 [{kernel,KernelVer},
	  {stdlib,StdlibVer} | OtherAppsRel]},
    ?line io:format(Fd,"~p.~n",[RelfileContent]),
    ?line ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)}.

create_include_files(inc1, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc1),
    create_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t6, \"1.0\"}, {t5, \"1.0\"}, \n"
          "  {t4, \"1.0\"}, {t3, \"1.0\"}, {t2, \"1.0\"}, \n"
          "  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc2, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc2),
    create_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t6 does not include t5 !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t6, \"1.0\", [t4]}, {t5, \"1.0\"}, \n"
          "  {t4, \"1.0\"}, {t3, \"1.0\"}, {t2, \"1.0\"}, \n"
          "  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc3, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc3),
    create_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t3 does not include t2 !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t6, \"1.0\"}, {t5, \"1.0\"}, \n"
          "  {t4, \"1.0\"}, {t3, \"1.0\", []}, {t2, \"1.0\"}, \n"
          "  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc4, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc4),
    create_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t3 does not include t2 !
    %% t6 does not include t5 !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t6, \"1.0\", [t4]}, {t5, \"1.0\"}, \n"
          "  {t4, \"1.0\"}, {t3, \"1.0\", []}, {t2, \"1.0\"}, \n"
          "  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc5, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc5),
    create_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t6 does not include t5 !
    %% exclude t5.
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t6, \"1.0\", [t4]}, \n"
          "  {t4, \"1.0\"}, {t3, \"1.0\", []}, {t2, \"1.0\"}, \n"
          "  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc6, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc6),
    create_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t3 does include non existing t2 !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t6, \"1.0\"}, {t5, \"1.0\"}, \n"
          "  {t4, \"1.0\"}, {t3, \"1.0\"}, \n"
          "  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc7, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc7),
    create_apps(PrivDir),
    create_app(t7, PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t7 and t6 does include t5 !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t7, \"1.0\"}, {t6, \"1.0\"}, {t5, \"1.0\"}, \n"
          "  {t4, \"1.0\"}, {t3, \"1.0\"}, {t2, \"1.0\"}, \n"
          "  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc8, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc8),
    create_circular_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t8 uses t9 and t10 includes t9 !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t8, \"1.0\"}, {t9, \"1.0\"}, {t10, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc9, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc9),
    create_circular_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t8 uses t9, t9 uses t10 and t10 includes t8 ==> circular !!
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t8, \"1.0\"}, {t9, \"1.0\"}, {t10, \"1.0\", [t8]}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc10, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc10),
    create_circular_apps(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t9 tries to include not specified (in .app file) application !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t8, \"1.0\"}, {t9, \"1.0\", [t7]}, {t10, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc11, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, inc11),
    create_apps2(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {t11, \"1.0\"}, \n"
          "  {t12, \"1.0\"}, \n"
          "  {t13, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(otp_3065, Config) ->
    ?line PrivDir = ?privdir,
    ?line Name = fname(PrivDir, otp_3065),
    create_apps_3065(PrivDir),

    ?line Apps = application_controller:which_applications(),
    ?line {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    ?line {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
          " [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
           ++ StdlibVer ++ "\"},\n"
          "  {chAts, \"1.0\"}, {aa12, \"1.0\"}, \n"
          "  {chTraffic, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)}.

create_apps(Dir) ->
    T1 = "{application, t1,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [kernel, stdlib]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't1.app'), list_to_binary(T1)),

    T2 = "{application, t2,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [t1]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't2.app'), list_to_binary(T2)),

    T3 = "{application, t3,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, [t2]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't3.app'), list_to_binary(T3)),

    T4 = "{application, t4,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [t3]},\n"
         "  {included_applications, []},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't4.app'), list_to_binary(T4)),

    T5 = "{application, t5,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [t3]},\n"
         "  {included_applications, []},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't5.app'), list_to_binary(T5)),

    T6 = "{application, t6,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, [t4, t5]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't6.app'), list_to_binary(T6)).

create_app(t7, Dir) ->
    T7 = "{application, t7,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, [t5]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't7.app'), list_to_binary(T7)).

create_circular_apps(Dir) ->
    T8 = "{application, t8,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [t9]},\n"
         "  {included_applications, []},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't8.app'), list_to_binary(T8)),

    T9 = "{application, t9,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [t10]},\n"
         "  {included_applications, []},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't9.app'), list_to_binary(T9)),

    T10 = "{application, t10,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, [t8, t9]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't10.app'), list_to_binary(T10)).

create_apps2(Dir) ->
    T11 = "{application, t11,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, [t13]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't11.app'), list_to_binary(T11)),

    T12 = "{application, t12,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [t11]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't12.app'), list_to_binary(T12)),

    T13 = "{application, t13,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, []},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't13.app'), list_to_binary(T13)).



create_apps_3065(Dir) ->
    T11 = "{application, chTraffic,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, [chAts]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 'chTraffic.app'), list_to_binary(T11)),

    T12 = "{application, chAts,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, []},\n"
         "  {included_applications, [aa12]},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 'chAts.app'), list_to_binary(T12)),

    T13 = "{application, aa12,\n"
         " [{vsn, \"1.0\"},\n"
         "  {description, \"test\"},\n"
	 "  {modules, []},\n"
         "  {applications, [chAts]},\n"
         "  {included_applications, []},\n"
	 "  {registered, []}]}.\n",
    file:write_file(fname(Dir, 'aa12.app'), list_to_binary(T13)).

fname(N) ->
    filename:join(N).

fname(Dir, Basename) ->
    filename:join(Dir, Basename).
