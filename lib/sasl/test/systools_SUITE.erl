%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
%%
%% Test suite for the systools module.
%%
%%	The systools module is a wrapper for a number of modules that
%%	handle large parts of the release building functionality
%%	(e.g. checking app files, building a tar file, building
%%	release upgrad scripts.
%%


-module(systools_SUITE).

-compile(export_all).

%%-define(debug, true).

-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-define(copydir, ?config(copy_dir, Config)).

-include_lib("kernel/include/file.hrl").

-import(lists, [foldl/3]).

-define(default_timeout, ?t:minutes(20)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [{group, script}, {group, tar}, {group, relup}, {group, hybrid},
     {group, options}].

groups() -> 
    [{script, [],
      [script_options, normal_script, unicode_script, no_mod_vsn_script,
       wildcard_script, variable_script, abnormal_script,
       no_sasl_script, no_dot_erlang_script,
       src_tests_script, crazy_script,
       included_script, included_override_script,
       included_fail_script, included_bug_script, exref_script,
       duplicate_modules_script,
       otp_3065_circular_dependenies, included_and_used_sort_script]},
     {tar, [],
      [tar_options, normal_tar, no_mod_vsn_tar, system_files_tar,
       system_src_file_tar, invalid_system_files_tar, variable_tar,
       src_tests_tar, var_tar, exref_tar, link_tar, no_sasl_tar,
       otp_9507_path_ebin]},
     {relup, [],
      [normal_relup, restart_relup, abnormal_relup, no_sasl_relup,
       no_appup_relup, bad_appup_relup, app_start_type_relup, regexp_relup
      ]},
     {hybrid, [], [normal_hybrid,hybrid_no_old_sasl,hybrid_no_new_sasl]},
     {options, [], [otp_6226_outdir,app_file_defaults]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_per_suite(Config) when is_list(Config) ->
    %% To use in end_per_testcase
    Path = code:get_path(),
    {ok,Cwd} = file:get_cwd(),

    %% Make of copy of the data directory.
    DataDir = ?datadir,
    PrivDir = ?privdir,
    CopyDir = fname(PrivDir, "datacopy"),
    ok = file:make_dir(CopyDir),
    TarFile = fname(PrivDir, "datacopy.tgz"),
    ok = file:set_cwd(DataDir),
    ok = erl_tar:create(TarFile, ["."], [compressed]),
    ok = erl_tar:extract(TarFile, [compressed, {cwd,CopyDir}]),
    ok = file:delete(TarFile),

    %% Compile source files in the copy directory.
    Sources = filelib:wildcard(fname([CopyDir,'*','*','*','*','*.erl'])),
    lists:foreach(fun compile_source/1, Sources),

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

end_per_suite(Config) when is_list(Config) ->
    rh_test_lib:clean_dir(?privdir),
    Config.

init_per_testcase(link_tar, Config) ->
    case os:type() of
	{unix, _} -> init_per_testcase(dummy, Config);
	{win32, _} -> {skip, "Skip on windows"}
    end;
init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
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


%% make_script: Check illegal script options
script_options(Config) when is_list(Config) ->
    {'EXIT',{{badarg,[{path,["Path",12,"Another"]}]}, _}} =
	(catch systools:make_script("release", [{path,["Path",12,"Another"]}])),
    {'EXIT',{{badarg,[sillent]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path","Another"]},sillent])),
    {'EXIT',{{badarg,[locall]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path","Another"]},locall])),
    {'EXIT',{{badarg,[src_testsxx]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path"]},src_testsxx])),
    {'EXIT',{{badarg,[{variables, {"TEST", "/home/lib"}}]}, _}} =
	(catch systools:make_script("release",
				    [{variables, {"TEST", "/home/lib"}}])),
    {'EXIT',{{badarg,[{variables, [{a, b}, {"a", "b"}]}]}, _}} =
	(catch systools:make_script("release",
				    [{variables, [{a, b}, {"a", "b"}]}])),
    {'EXIT',{{badarg,[exreff]}, _}} =
	(catch systools:make_script("release",
				    [{path,["Path","Another"]},exreff])),
    {'EXIT',{{badarg,[{exref,["appl"]}]}, _}} =
	(catch systools:make_script("release", [{exref,["appl"]}])),
    {'EXIT',{{badarg,[{machine, "appl"}]}, _}} =
	(catch systools:make_script("release", [{machine,"appl"}])),
    ok.


%% make_script: Check that normal case
normal_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    PSAVE = code:get_path(),		% Save path

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P1 = fname([LibDir, 'db-2.1', ebin]),
    P2 = fname([LibDir, 'fe-3.1', ebin]),

    true = code:add_patha(P1),
    true = code:add_patha(P2),

    ok = file:set_cwd(LatestDir),

    ok = systools:make_script(filename:basename(LatestName)),
    {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Check the same but w. silent flag
    {ok, _, []} = systools:make_script(LatestName, [silent]),
    {ok, _, []} = systools:make_script(LatestName, [silent,warnings_as_errors]),

    %% Use the local option
    ok = systools:make_script(LatestName, [local]),
    ok = check_script_path(LatestName),

    %% use the path option
    code:set_path(PSAVE),			% Restore path
    %% Mess up std path:
    true = code:add_patha(fname([LibDir, 'db-1.0', ebin])),
    true = code:add_patha(fname([LibDir, 'fe-2.1', ebin])),

    error = systools:make_script(LatestName),	%should fail
    ok = systools:make_script(LatestName,[{path, [P1, P2]}]),

    ok = file:set_cwd(OldDir),
    code:set_path(PSAVE),			% Restore path
    ok.


%% make_script: Test make_script with unicode .app file
unicode_script(Config) when is_list(Config) ->
    UnicodeStr = [945,946], % alhpa beta in greek letters

    {LatestDir, LatestName} = create_script({unicode,UnicodeStr},Config),

    DataDir = filename:absname(?copydir),
    UnicodeApp = fname([DataDir, "d_unicode", "lib", "ua-1.0"]),
    TarFile = fname(?privdir, "unicode_app.tgz"),
    {ok, Tar} = erl_tar:open(TarFile, [write, compressed]),
    ok = erl_tar:add(Tar, UnicodeApp, "ua-1.0", [compressed]),
    ok = erl_tar:close(Tar),

    UnicodeLibDir = fname([DataDir, "d_unicode", UnicodeStr]),
    P1 = fname([UnicodeLibDir, "ua-1.0", "ebin"]),

    %% Need to do this on a separate node to make sure it has unicode
    %% filename mode (+fnu*)
    {ok,HostStr} = inet:gethostname(),
    Host = list_to_atom(HostStr),
    {ok,Node} = ct_slave:start(Host,unicode_script_node,[{erl_flags,"+fnui"}]),

    ok = rpc:call(Node,erl_tar,extract,
		  [TarFile, [{cwd,UnicodeLibDir},compressed]]),

    true = rpc:call(Node,code,add_patha,[P1]),

    ok = rpc:call(Node,file,set_cwd,[LatestDir]),

    ok = rpc:call(Node,systools,make_script,[filename:basename(LatestName),
					     [local]]),

    {ok, Script} = rpc:call(Node,file,consult,[LatestName++".script"]),

    %% For debug purpose - print script to log
    io:format("~tp~n",[Script]),

    %% check that script contains unicode strings in
    %% 1. release version (set in ?MODULE:do_create_script)
    [{script,{"Test release",UnicodeStr},Instr}] = Script,

    %% 2. application description (set in ua.app in data dir)
    [AppInfo] = [X || {apply,{application,load,[{application,ua,X}]}} <- Instr],
    {description,UnicodeStr} = lists:keyfind(description,1,AppInfo),

    %% 3. path (directory name where unicode_app.tgz is extracted)
    true = lists:member({path,[P1]},Instr),

    %% If all is good, delete the unicode dir to avoid lingering files
    %% on windows.
    rpc:call(Node,code,add_pathz,[filename:dirname(code:which(?MODULE))]),
    rpc:call(Node,?MODULE,delete_tree,[UnicodeLibDir]),

    ok.

unicode_script(cleanup,Config) ->
    _ = ct_slave:stop(unicode_script_node),
    file:delete(fname(?privdir, "unicode_app.tgz")),
    ok.


%% make_script:
%% Modules specified without version in .app file (db-3.1).
%% Note that this is now the normal way - i.e. systools now ignores
%% the module versions in the .app file.
no_mod_vsn_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    PSAVE = code:get_path(),		% Save path

    {LatestDir, LatestName} = create_script(latest_no_mod_vsn,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P1 = fname([LibDir, 'db-3.1', ebin]),
    P2 = fname([LibDir, 'fe-3.1', ebin]),

    true = code:add_patha(P1),
    true = code:add_patha(P2),

    ok = file:set_cwd(LatestDir),

    ok = systools:make_script(filename:basename(LatestName)),
    {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Check the same but w. silent flag
    {ok, _, []} = systools:make_script(LatestName, [silent]),

    %% Use the local option
    ok = systools:make_script(LatestName, [local]),
    ok = check_script_path(LatestName),

    %% use the path option
    code:set_path(PSAVE),			% Restore path
    %% Mess up std path:
    true = code:add_patha(fname([LibDir, 'db-1.0', ebin])),
    true = code:add_patha(fname([LibDir, 'fe-2.1', ebin])),

    error = systools:make_script(LatestName),	%should fail
    ok = systools:make_script(LatestName,
			      [{path, [P1, P2]}]),

    ok = file:set_cwd(OldDir),
    code:set_path(PSAVE),			% Restore path
    ok.


%% make_script: Check that make_script handles wildcards in path.
wildcard_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    WildDir = fname([LibDir, '*', ebin]),

    ok = file:set_cwd(LatestDir),

    error = systools:make_script(filename:basename(LatestName)),

    ok = systools:make_script(LatestName,
			      [{path, [WildDir]}]),

    {ok, _} = read_script_file(LatestName),	% Check readabillity

    ok = file:set_cwd(OldDir),
    ok.


%% make_script: Add own installation dependent variable in script.
variable_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    ok = systools:make_script(LatestName,
			      [{path, P},
			       {variables, [{"TEST", LibDir}]}]),

    %% Check variables
    ok = check_var_script_file([fname(['$TEST', 'db-2.1', ebin]),
				fname(['$TEST', 'fe-3.1', ebin])],
			       P,
			       LatestName),

    ok = file:set_cwd(OldDir),
    ok.

%% make_script: Abnormal cases.
abnormal_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),

    ok = file:set_cwd(LatestDir),
    LibDir = fname([DataDir, d_bad_app_vsn, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    %% Check wrong app vsn
    error = systools:make_script(LatestName, [{path, P}]),
    {error,
     systools_make,
     [{error_reading, {db, {no_valid_version,
			    {{"should be","2.1"},
			     {"found file", _, "2.0"}}}}}]} =
	systools:make_script(LatestName, [silent, {path, P}]),

    ok = file:set_cwd(OldDir),
    ok.


%% make_script: Create script without sasl appl. Check warning.
no_sasl_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest1_no_sasl,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _ , [{warning,missing_sasl}]} =
	systools:make_script(LatestName,[{path, P},silent]),

    {error, systools_make, {warnings_treated_as_errors,[missing_sasl]}} =
	systools:make_script(LatestName,[{path, P},silent,warnings_as_errors]),

    {ok, _ , []} =
	systools:make_script(LatestName,[{path, P},silent, no_warn_sasl]),

    {ok, _ , []} =
	systools:make_script(LatestName,[{path, P},silent, no_warn_sasl,
                                         warnings_as_errors]),

    ok = file:set_cwd(OldDir),
    ok.

%% make_script: Create script with no_dot_erlang. Check script contents.
no_dot_erlang_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest1_no_sasl,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _ , []} =
	systools:make_script(LatestName,[{path, P},silent, no_warn_sasl]),
    {ok, [{_, _, LoadDotErlang}]} = read_script_file(LatestName),
    [erlangrc] = [E || {apply, {c, E, []}} <- LoadDotErlang],

    {ok, _ , []} =
	systools:make_script(LatestName,[{path, P},silent, no_warn_sasl, no_dot_erlang]),
    {ok, [{_, _, DoNotLoadDotErlang}]} = read_script_file(LatestName),
    [] = [E || {apply, {c, E, []}} <- DoNotLoadDotErlang],

    ok = file:set_cwd(OldDir),
    ok.


%% make_script: Do not check date of object file or that source code
%% can be found.
src_tests_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    PSAVE = code:get_path(),		% Save path

    {LatestDir, LatestName} = create_script(latest,Config),
    BootFile = LatestName ++ ".boot",

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_missing_src, lib]),
    P1 = fname([LibDir, 'db-2.1', ebin]),
    P2 = fname([LibDir, 'fe-3.1', ebin]),
    N = [P1, P2],

    ok = file:set_cwd(LatestDir),

    %% Manipulate the modification date of a beam file so it seems
    %% older than its .erl file
    Erl = filename:join([P1,"..","src","db1.erl"]),
    {ok, FileInfo=#file_info{mtime={{Y,M,D},T}}} = file:read_file_info(Erl),
    Beam = filename:join(P1,"db1.beam"),
    ok=file:write_file_info(Beam, FileInfo#file_info{mtime={{Y-1,M,D},T}}),

    %% Remove a .erl file
    Erl2 = filename:join([P1,"..","src","db2.erl"]),
    file:delete(Erl2),

    %% Then make script

    %% .boot file should not exist
    ok = file:delete(BootFile),
    false = filelib:is_regular(BootFile),
    %% With warnings_as_errors and src_tests option, an error should be issued
    {error, systools_make,
     {warnings_treated_as_errors, [{obj_out_of_date,_},
                                   {source_not_found,_}]}} =
	systools:make_script(LatestName, [silent, {path, N}, src_tests,
					  warnings_as_errors]),
    error =
	systools:make_script(LatestName, [{path, N}, src_tests,
					  warnings_as_errors]),

    %% due to warnings_as_errors .boot file should still not exist
    false = filelib:is_regular(BootFile),

    %% Two warnings should be issued when src_tests is given
    %% 1. old object code for db1.beam
    %% 2. missing source code for db2.beam
    {ok, _, [{warning,{obj_out_of_date,_}},
	     {warning,{source_not_found,_}}]} =
	systools:make_script(LatestName, [silent, {path, N}, src_tests]),

    %% .boot file should exist now
    true = filelib:is_regular(BootFile),

    %% Without the src_tests option, no warning should be issued
    {ok, _, []} =
	systools:make_script(LatestName, [silent, {path, N}]),

    %% Check that the old no_module_tests option (from the time when
    %% it was default to do the src_test) is ignored
    {ok, _, [{warning,{obj_out_of_date,_}},
	     {warning,{source_not_found,_}}]} =
	systools:make_script(LatestName, [silent,
					  {path, N},
					  no_module_tests,
					  src_tests]),

    ok = file:set_cwd(OldDir),
    code:set_path(PSAVE),
    ok.

%% make_script: Do the crazy cases.
crazy_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest, Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    %% Run with bad path
    error = systools:make_script(LatestName),
    {error, _, [{error_reading, _}, {error_reading, _}]} =
	systools:make_script(LatestName, [silent]),

    %% Run with .rel file lacking kernel
    {LatestDir2, LatestName2} = create_script(latest_nokernel, Config),
    ok = file:set_cwd(LatestDir2),

    error = systools:make_script(LatestName2),
    {error, _, {missing_mandatory_app,kernel}} =
	systools:make_script(LatestName2, [silent,{path,P}]),

    %% Run with .rel file with non-permanent kernel
    {LatestDir3, LatestName3} = create_script(latest_kernel_start_type, Config),
    ok = file:set_cwd(LatestDir3),

    error = systools:make_script(LatestName3),
    {error, _, {mandatory_app,kernel,load}} =
	systools:make_script(LatestName3, [silent,{path,P}]),

    %% Run with .rel file with non-permanent stdlib
    {LatestDir4, LatestName4} = create_script(latest_stdlib_start_type, Config),
    ok = file:set_cwd(LatestDir4),

    error = systools:make_script(LatestName4),
    {error, _, {mandatory_app,stdlib,load}} =
	systools:make_script(LatestName4, [silent,{path,P}]),

    %% Run with .rel file lacking stdlib
    {LatestDir5, LatestName5} = create_script(latest_no_stdlib, Config),
    ok = file:set_cwd(LatestDir5),

    error = systools:make_script(LatestName5),
    {error, _, {missing_mandatory_app,stdlib}} =
	systools:make_script(LatestName5, [silent,{path,P}]),

    ok = file:set_cwd(OldDir),
    ok.

%% make_script: Check that make_script handles generation of script
%% for applications with included applications.
included_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir, LatestName} = create_include_files(inc1, Config),
    ok = file:set_cwd(LatestDir),
    ok = systools:make_script(LatestName),
    ok = check_include_script(LatestName,
			      [t1, t2, t3, t5, t4, t6],
			      [t1, t3, t6]),
    ok = file:set_cwd(OldDir),
    ok.

%% make_script: Check that make_script handles generation of script
%% for applications with included applications which are override by
%% the .rel file.
included_override_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir, LatestName} = create_include_files(inc2, Config),
    ok = file:set_cwd(LatestDir),
    ok = systools:make_script(LatestName),
    ok = check_include_script(LatestName,
			      [t1, t2, t3, t4, t6, t5],
			      [t1, t3, t6, t5]),

    {_, LatestName1} = create_include_files(inc3, Config),
    ok = systools:make_script(LatestName1),
    ok = check_include_script(LatestName1,
			      [t3, t5, t4, t6, t1, t2],
			      [t3, t6, t1, t2]),

    {_, LatestName2} = create_include_files(inc4, Config),
    ok = systools:make_script(LatestName2),
    ok = check_include_script(LatestName2,
			      [t3, t4, t6, t5, t1, t2],
			      [t3, t6, t5, t1, t2]),

    {_, LatestName3} = create_include_files(inc5, Config),
    ok = systools:make_script(LatestName3),
    ok = check_include_script(LatestName3,
			      [t3, t4, t6, t1, t2],
			      [t3, t6, t1, t2]),

    ok = file:set_cwd(OldDir),
    ok.

%% make_script: Check that make_script handles errors then generating
%% script with included applications.
included_fail_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir, LatestName} = create_include_files(inc6, Config),
    ok = file:set_cwd(LatestDir),
    {error, _, {undefined_applications,[t2]}} =
	systools:make_script(LatestName, [silent]),

    {_, LatestName1} = create_include_files(inc7, Config),
    {error, _, {duplicate_include,[{{t5,t7,_,_},{t5,t6,_,_}}]}} =
	systools:make_script(LatestName1, [silent]),

    {_, LatestName3} = create_include_files(inc9, Config),
    {error, _, {circular_dependencies,[{t10,_},{t8,_}]}} =
	systools:make_script(LatestName3, [silent]),

    {_, LatestName4} = create_include_files(inc10, Config),
    {error, _, [{error_reading,{t9,{override_include,[t7]}}}]} =
	systools:make_script(LatestName4, [silent]),

    ok = file:set_cwd(OldDir),
    ok.

%% make_script: Check that make_script handles generation of script
%% with difficult dependency for included applications.
included_bug_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir, LatestName} = create_include_files(inc11, Config),
    ok = file:set_cwd(LatestDir),
    ok = systools:make_script(LatestName),
    ok = check_include_script(LatestName,
			      [t13, t11, t12],
			      [t11, t12]),
    ok = file:set_cwd(OldDir),
    ok.


%% make_script: Circular dependencies in systools:make_script().
otp_3065_circular_dependenies(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir, LatestName} =
	create_include_files(otp_3065_circular_dependenies, Config),
    ok = file:set_cwd(LatestDir),
    ok = systools:make_script(LatestName),
    ok = check_include_script(LatestName,
			      [aa12, chAts, chTraffic],
			      [chTraffic]),
    ok = file:set_cwd(OldDir),
    ok.

%% Test sorting of included applications and used applications
included_and_used_sort_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir1, LatestName1} = create_include_files(sort_apps, Config),
    ok = file:set_cwd(LatestDir1),
    ok = systools:make_script(LatestName1),
    ok = check_include_script(LatestName1,
			      [t20,t19,t18,t17,t16,t15,t14],[t20,t19,t18,t14]),

    {LatestDir2, LatestName2} = create_include_files(sort_apps_rev, Config),
    ok = file:set_cwd(LatestDir2),
    ok = systools:make_script(LatestName2),
    ok = check_include_script(LatestName2,
			      [t18,t19,t20,t15,t16,t17,t14],[t18,t19,t20,t14]),

    ok = file:set_cwd(OldDir),
    ok.


%% make_script: Check that make_script exref option works.
exref_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    PSAVE = code:get_path(),		% Save path

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, []} = systools:make_script(LatestName, [{path,P}, silent]),

    %% Complete exref
    {ok, _, W1} =
	systools:make_script(LatestName, [exref, {path,P}, silent]),
    check_exref_warnings(with_db1, W1),
    {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Only exref the db application.
    {ok, _, W2} =
	systools:make_script(LatestName, [{exref,[db]}, {path,P}, silent]),
    check_exref_warnings(with_db1, W2),
    {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% Only exref the fe application.
    {ok, _, W3} =
	systools:make_script(LatestName, [{exref,[fe]}, {path,P}, silent]),
    check_exref_warnings(without_db1, W3),
    {ok, _} = read_script_file(LatestName),	% Check readabillity

    %% exref the db and stdlib applications.
    {ok, _, W4} =
	systools:make_script(LatestName, [{exref,[db,stdlib]}, {path,P}, silent]),
    check_exref_warnings(with_db1, W4),
    {ok, _} = read_script_file(LatestName),	% Check readabillity
    ok = file:set_cwd(OldDir),
    code:set_path(PSAVE),			% Restore path
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

%% duplicate_modules_script: Check that make_script rejects two
%% applications providing the same module.
duplicate_modules_script(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(duplicate_modules,Config),

    DataDir = filename:absname(?copydir),

    ok = file:set_cwd(LatestDir),
    LibDir = fname([DataDir, d_duplicate_modules, lib]),
    P = [fname([LibDir, 'app1-1.0', ebin]),
	 fname([LibDir, 'app2-1.0', ebin])],

    %% Check wrong app vsn
    error = systools:make_script(LatestName, [{path, P}]),
    {error,
      systools_make,
      {duplicate_modules, [
          {{myapp,app1,_}, {myapp,app2,_}}
        ]
      }
    } = systools:make_script(LatestName, [silent, {path, P}]),

    ok = file:set_cwd(OldDir),
    ok.

%% tar_options: Check illegal tar options.
tar_options(Config) when is_list(Config) ->
    {'EXIT',{{badarg,[{path,["Path",12,"Another"]}]}, _}} =
	(catch systools:make_tar("release", [{path,["Path",12,"Another"]}])),
    {'EXIT',{{badarg,[sillent]}, _}} =
	(catch systools:make_tar("release",
				 [{path,["Path","Another"]},sillent])),
    {'EXIT',{{badarg,[{dirs,["dirs"]}]}, _}} =
	(catch systools:make_tar("release", [{dirs, ["dirs"]}])),
    {'EXIT',{{badarg,[{erts, illegal}]}, _}} =
	(catch systools:make_tar("release", [{erts, illegal}])),
    {'EXIT',{{badarg,[src_testsxx]}, _}} =
	(catch systools:make_tar("release",
				 [{path,["Path"]},src_testsxx])),
    {'EXIT',{{badarg,[{variables, [{a, b}, {"a", "b"}]}]}, _}} =
	(catch systools:make_tar("release",
				 [{variables, [{a, b}, {"a", "b"}]}])),
    {'EXIT',{{badarg,[{var_tar, illegal}]}, _}} =
	(catch systools:make_tar("release", [{var_tar, illegal}])),
    {'EXIT',{{badarg,[exreff]}, _}} =
	(catch systools:make_tar("release",
				 [{path,["Path","Another"]},exreff])),
    {'EXIT',{{badarg,[{exref,["appl"]}]}, _}} =
	(catch systools:make_tar("release", [{exref,["appl"]}])),
    {'EXIT',{{badarg,[{machine, "appl"}]}, _}} =
	(catch systools:make_tar("release", [{machine,"appl"}])),
    ok.


%% make_tar: Check normal case
normal_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, []} = systools:make_script(LatestName, [silent, {path, P}]),
    ok = systools:make_tar(LatestName, [{path, P}]),
    ok = check_tar(fname([lib,'db-2.1',ebin,'db.app']), LatestName),
    {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent]),
    ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    ok = file:set_cwd(OldDir),
    ok.

%% make_tar: Modules specified without version in .app file (db-3.1).
%% Note that this is now the normal way - i.e. systools now ignores
%% the module versions in the .app file.
no_mod_vsn_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest_no_mod_vsn,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-3.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, []} = systools:make_script(LatestName, [silent, {path, P}]),
    ok = systools:make_tar(LatestName, [{path, P}]),
    ok = check_tar(fname([lib,'db-3.1',ebin,'db.app']), LatestName),
    {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent]),
    ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    ok = file:set_cwd(OldDir),
    ok.


%% make_tar: Check that relup or sys.config are included if they exist
system_files_tar(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    %% Add dummy relup and sys.config
    ok = file:write_file("sys.config","[].\n"),
    ok = file:write_file("relup","{\"LATEST\",[],[]}.\n"),

    {ok, _, []} = systools:make_script(LatestName, [silent, {path, P}]),
    ok = systools:make_tar(LatestName, [{path, P}]),
    ok = check_tar(fname(["releases","LATEST","sys.config"]), LatestName),
    ok = check_tar(fname(["releases","LATEST","relup"]), LatestName),
    {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent]),
    ok = check_tar(fname(["releases","LATEST","sys.config"]), LatestName),
    ok = check_tar(fname(["releases","LATEST","relup"]), LatestName),

    ok = file:set_cwd(OldDir),

    ok.


system_files_tar(cleanup,Config) ->
    Dir = ?privdir,
    file:delete(filename:join(Dir,"sys.config")),
    file:delete(filename:join(Dir,"relup")),
    ok.

%% make_tar: Check that sys.config.src and not sys.config is included
system_src_file_tar(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
         fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    %% Add dummy sys.config and sys.config.src
    ok = file:write_file("sys.config.src","[${SOMETHING}].\n"),
    ok = file:write_file("sys.config","[].\n"),

    {ok, _, _} = systools:make_script(LatestName, [silent, {path, P}]),
    ok = systools:make_tar(LatestName, [{path, P}]),
    ok = check_tar(fname(["releases","LATEST","sys.config.src"]), LatestName),
    {error, _} = check_tar(fname(["releases","LATEST","sys.config"]), LatestName),
    {ok, _, _} = systools:make_tar(LatestName, [{path, P}, silent]),
    ok = check_tar(fname(["releases","LATEST","sys.config.src"]), LatestName),
    {error, _} = check_tar(fname(["releases","LATEST","sys.config"]), LatestName),

    ok = file:set_cwd(OldDir),

    ok.

system_src_file_tar(cleanup,Config) ->
    Dir = ?privdir,
    file:delete(filename:join(Dir,"sys.config")),
    file:delete(filename:join(Dir,"sys.config.src")),
    ok.

%% make_tar: Check that make_tar fails if relup or sys.config exist
%% but do not have valid content
invalid_system_files_tar(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, []} = systools:make_script(LatestName, [silent, {path, P}]),

    %% Add dummy relup and sys.config - faulty sys.config
    ok = file:write_file("sys.config","[]\n"), %!!! syntax error - missing '.'
    ok = file:write_file("relup","{\"LATEST\",[],[]}.\n"),

    error = systools:make_tar(LatestName, [{path, P}]),
    {error,_,{tar_error,{add,"sys.config",[{error,_}]}}} =
	systools:make_tar(LatestName, [{path, P}, silent]),

    %% Add dummy relup and sys.config - faulty sys.config
    ok = file:write_file("sys.config","[x,y].\n"), %!!! faulty format
    ok = file:write_file("relup","{\"LATEST\",[],[]}.\n"),

    error = systools:make_tar(LatestName, [{path, P}]),
    {error,_,{tar_error,{add,"sys.config",[invalid_format]}}} =
	systools:make_tar(LatestName, [{path, P}, silent]),

    %% Add dummy relup and sys.config - faulty relup
    ok = file:write_file("sys.config","[]\n"),
    ok = file:write_file("relup","{\"LATEST\"\n"), %!!! syntax error - truncated

    error = systools:make_tar(LatestName, [{path, P}]),
    {error,_,{tar_error,{add,"relup",[{error,_}]}}} =
	systools:make_tar(LatestName, [{path, P}, silent]),

    %% Add dummy relup and sys.config - faulty relup
    ok = file:write_file("sys.config","[]\n"),
    ok = file:write_file("relup","[].\n"), %!!! faulty format

    error = systools:make_tar(LatestName, [{path, P}]),
    {error,_,{tar_error,{add,"relup",[invalid_format]}}} =
	systools:make_tar(LatestName, [{path, P}, silent]),

    ok = file:set_cwd(OldDir),

    ok.

invalid_system_files_tar(cleanup,Config) ->
    Dir = ?privdir,
    file:delete(filename:join(Dir,"sys.config")),
    file:delete(filename:join(Dir,"relup")),
    ok.


%% make_tar: Use variable and create separate tar (included in generated tar).
variable_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, []} = systools:make_script(LatestName,
				      [silent,
				       {path, P},
				       {variables,[{"TEST", LibDir}]}]),

    ok = systools:make_tar(LatestName, [{path, P},
					{variables,[{"TEST", LibDir}]}]),
    ok = check_var_tar("TEST", LatestName),

    {ok, _, []} = systools:make_tar(LatestName,
				   [{path, P}, silent,
				    {variables,[{"TEST", LibDir}]}]),
    ok = check_var_tar("TEST", LatestName),

    ok = file:set_cwd(OldDir),
    ok.

%% make_tar: Check that symlinks in applications are handled correctly.
link_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_links, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    %% Make some links
    Db1Erl = fname(['db-2.1',src,'db1.erl']),
    NormalDb1Erl = fname([DataDir,d_normal,lib,Db1Erl]),
    LinkDb1Erl = fname([LibDir, Db1Erl]),
    ok = file:make_symlink(NormalDb1Erl, LinkDb1Erl),
    Db1Beam = fname(['db-2.1',ebin,'db1.beam']),
    NormalDb1Beam = fname([DataDir,d_normal,lib,Db1Beam]),
    LinkDb1Beam = fname([LibDir, Db1Beam]),
    ok = file:make_symlink(NormalDb1Beam, LinkDb1Beam),
    FeApp = fname(['fe-3.1',ebin,'fe.app']),
    NormalFeApp = fname([DataDir,d_normal,lib,FeApp]),
    LinkFeApp = fname([LibDir, FeApp]),
    ok = file:make_symlink(NormalFeApp, LinkFeApp),

    %% Create the tar and check that the linked files are included as
    %% regular files
    ok = file:set_cwd(LatestDir),

    {ok,_,[]} = systools:make_script(LatestName, [{path, P},silent]),

    {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent]),
    ok = check_tar_regular(?privdir,
			   [fname([lib,FeApp]),
			    fname([lib,Db1Beam])],
			   LatestName),

    {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent,
						 {dirs, [src]}]),
    ok = check_tar_regular(?privdir,
			   [fname([lib,FeApp]),
			    fname([lib,Db1Beam]),
			    fname([lib,Db1Erl])],
			   LatestName),

    ok = file:set_cwd(OldDir),
    ok.

%% make_tar: Do not check date of object file or that source code can be found.
src_tests_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_missing_src, lib]),
    P1 = fname([LibDir, 'db-2.1', ebin]),
    P2 = fname([LibDir, 'fe-3.1', ebin]),
    P = [P1, P2],

    ok = file:set_cwd(LatestDir),

    %% Manipulate the modification date of a beam file so it seems
    %% older than the .erl file
    Erl = filename:join([P1,"..","src","db1.erl"]),
    {ok, FileInfo=#file_info{mtime={{Y,M,D},T}}} = file:read_file_info(Erl),
    Beam = filename:join(P1,"db1.beam"),
    ok = file:write_file_info(Beam, FileInfo#file_info{mtime={{Y-1,M,D},T}}),

    %% Remove a .erl file
    Erl2 = filename:join([P1,"..","src","db2.erl"]),
    file:delete(Erl2),

    ok = systools:make_script(LatestName, [{path, P}]),

    %% Then make tar - two warnings should be issued when
    %% src_tests is given
    %% 1. old object code for db1.beam
    %% 2. missing source code for db2.beam
    {ok, _, [{warning,{obj_out_of_date,_}},
	     {warning,{source_not_found,_}}]} =
	systools:make_tar(LatestName, [{path, P}, silent,
				       {dirs, [src]},
				       src_tests]),
    ok = check_tar(fname([lib,'db-2.1',src,'db1.erl']), LatestName),

    %% Without the src_tests option, no warning should be issued
    {ok, _, []} = systools:make_tar(LatestName, [{path, P}, silent,
						 {dirs, [src]}]),
    ok = check_tar(fname([lib,'db-2.1',src,'db1.erl']), LatestName),

    %% Check that the old no_module_tests option (from the time when
    %% it was default to do the src_test) is ignored
    {ok, _, [{warning,{obj_out_of_date,_}},
	     {warning,{source_not_found,_}}]} =
	systools:make_tar(LatestName, [{path, P}, silent,
				       {dirs, [src]},
				       no_module_tests,
				       src_tests]),
    ok = check_tar(fname([lib,'db-2.1',src,'db1.erl']), LatestName),

    ok = file:set_cwd(OldDir),
    ok.


%% make_tar: Check that make_tar handles generation and placement of
%% tar files for variables outside the main tar file.
%% Test the {var_tar, include | ownfile | omit} optio.
var_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    PSAVE = code:get_path(),		% Save path

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, []} = systools:make_script(LatestName,
				      [silent,
				       {path, P},
				       {variables,[{"TEST", LibDir}]}]),

    ok = systools:make_tar(LatestName, [{path, P},
					{var_tar, ownfile},
					{variables,[{"TEST", LibDir}]}]),

    true = exists_tar_file("TEST"), %% Also removes the file !
    {error, {not_generated, _}} = check_var_tar("TEST", LatestName),

    ok = systools:make_tar(LatestName, [{path, P},
					{var_tar, omit},
					{variables,[{"TEST", LibDir}]}]),

    {error, {not_generated, _}} = check_var_tar("TEST", LatestName),
    false = exists_tar_file("TEST"),

    ok = systools:make_tar(LatestName, [{path, P},
					{var_tar, include},
					{variables,[{"TEST", LibDir}]}]),

    ok = check_var_tar("TEST", LatestName),
    false = exists_tar_file("TEST"),

    ok = file:set_cwd(OldDir),
    code:set_path(PSAVE),
    ok.


%% make_tar: Check exref option.
exref_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'fe-3.1', ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, []} = systools:make_script(LatestName, [silent, {path, P}]),

    %% Complete exref
    {ok, _, W1} =
	systools:make_tar(LatestName, [exref, {path, P}, silent]),
    check_exref_warnings(with_db1, W1),
    ok = check_tar(fname([lib,'db-2.1',ebin,'db.app']), LatestName),

    %% Only exref the db application.
    {ok, _, W2} =
	systools:make_tar(LatestName, [{exref, [db]}, {path, P}, silent]),
    check_exref_warnings(with_db1, W2),
    ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    %% Only exref the fe application.
    {ok, _, W3} =
	systools:make_tar(LatestName, [{exref, [fe]}, {path, P}, silent]),
    check_exref_warnings(without_db1, W3),
    ok = check_tar(fname([lib,'db-2.1',ebin,'db.app']), LatestName),

    %% exref the db and stdlib applications.
    {ok, _, W4} =
	systools:make_tar(LatestName, [{exref, [db, stdlib]},
				       {path, P}, silent]),
    check_exref_warnings(with_db1, W4),
    ok = check_tar(fname([lib,'fe-3.1',ebin,'fe.app']), LatestName),

    ok = file:set_cwd(OldDir),
    ok.

%% make_tar:  Create tar without sasl appl. Check warning.
no_sasl_tar(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest1_no_sasl,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),

    {ok, _, _} = systools:make_script(LatestName, [silent, {path, P}]),
    ok = systools:make_tar(LatestName, [{path, P}]),
    {ok, _, [{warning,missing_sasl}]} =
        systools:make_tar(LatestName, [{path, P}, silent]),
    {ok, _, []} =
        systools:make_tar(LatestName, [{path, P}, silent, no_warn_sasl]),
    {ok, _, []} =
        systools:make_tar(LatestName, [{path, P}, silent, no_warn_sasl,
                                       warnings_as_errors]),
    TarFile = LatestName ++ ".tar.gz",
    true = filelib:is_regular(TarFile),
    ok = file:delete(TarFile),
    {error, systools_make, {warnings_treated_as_errors,[missing_sasl]}} =
        systools:make_tar(LatestName, [{path, P}, silent, warnings_as_errors]),
    error =
        systools:make_tar(LatestName, [{path, P}, warnings_as_errors]),
    false = filelib:is_regular(TarFile),

    ok = file:set_cwd(OldDir),
    ok.

%% make_tar: OTP-9507 - make_tar failed when path given as just 'ebin'.
otp_9507_path_ebin(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest_small,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    FeDir = fname([LibDir, 'fe-3.1']),

    ok = file:set_cwd(FeDir),

    RelName = fname([LatestDir,LatestName]),

    P1 = ["./ebin",
	  fname([DataDir, lib, kernel, ebin]),
	  fname([DataDir, lib, stdlib, ebin]),
	  fname([DataDir, lib, sasl, ebin])],
    {ok, _, []} = systools:make_script(RelName, [silent, {path, P1}]),
    ok = systools:make_tar(RelName, [{path, P1}]),
    Content1 = tar_contents(RelName),

    P2 = ["ebin",
	  fname([DataDir, lib, kernel, ebin]),
	  fname([DataDir, lib, stdlib, ebin]),
	  fname([DataDir, lib, sasl, ebin])],

    %% Tickets solves the following line - it used to fail with
    %% {function_clause,[{filename,join,[[]]},...}
    ok = systools:make_tar(RelName, [{path, P2}]),
    Content2 = tar_contents(RelName),
    true = (Content1 == Content2),

    ok = file:set_cwd(OldDir),

    ok.


%% make_relup: Check normal case
normal_relup(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir,LatestName}   = create_script(latest0,Config),
    {_LatestDir1,LatestName1} = create_script(latest1,Config),
    {_LatestDir2,LatestName2} = create_script(latest2,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),

    %% This is the ultra normal case
    ok = systools:make_relup(LatestName, [LatestName1], [LatestName1],
			     [{path, P}]),
    ok = check_relup([{db, "2.1"}], [{db, "1.0"}]),
    {ok, Relup, _, []} =
	systools:make_relup(LatestName, [LatestName1], [LatestName1],
			    [{path, P}, silent]),
    ok = check_relup([{db, "2.1"}], [{db, "1.0"}]),

    %% file should not be written if warnings_as_errors is enabled.
    %% delete before running tests.
    ok = file:delete("relup"),

    %% Check that warnings are treated as errors
    error =
	systools:make_relup(LatestName, [LatestName2], [LatestName1],
			    [{path, P}, warnings_as_errors]),
    {error, systools_relup,
     {warnings_treated_as_errors,[pre_R15_emulator_upgrade,
                                  {erts_vsn_changed, _}]}} =
	systools:make_relup(LatestName, [LatestName2], [LatestName1],
			    [{path, P}, silent, warnings_as_errors]),

    %% relup file should not exist
    false = filelib:is_regular("relup"),

    %% Check that warnings get through
    ok = systools:make_relup(LatestName, [LatestName2], [LatestName1],
			     [{path, P}]),
    ok = check_relup([{fe, "3.1"}, {db, "2.1"}], [{db, "1.0"}]),
    {ok, _, _, [pre_R15_emulator_upgrade,{erts_vsn_changed, _}]} =
	systools:make_relup(LatestName, [LatestName2], [LatestName1],
			    [{path, P}, silent]),
    ok = check_relup([{fe, "3.1"}, {db, "2.1"}], [{db, "1.0"}]),

    %% relup file should exist now
    true = filelib:is_regular("relup"),

    %% file should not be written if noexec option is used.
    %% delete before running tests.
    ok = file:delete("relup"),
    {ok,Relup,_,[]} =
        systools:make_relup(LatestName, [LatestName1], [LatestName1],
                            [{path, P}, noexec]),
    false = filelib:is_regular("relup"),

    ok = file:set_cwd(OldDir),
    ok.


%% make_relup: Test relup which includes emulator restart.
restart_relup(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir,LatestName}   = create_script(latest0,Config),
    {_LatestDir1,LatestName1} = create_script(latest1,Config),
    {_LatestDir0CurrErts,LatestName0CurrErts} =
	create_script(latest0_current_erts,Config),
    {_CurrentAllDir,CurrentAllName} = create_script(current_all,Config),
    {_CurrentAllFutErtsDir,CurrentAllFutErtsName} =
	create_script(current_all_future_erts,Config),
    {_CurrentAllFutSaslDir,CurrentAllFutSaslName} =
	create_script(current_all_future_sasl,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin]),
	 fname([DataDir, lib, 'sasl-9.9', ebin])],

    ok = file:set_cwd(LatestDir),

    %% OTP-2561: Check that the option 'restart_emulator' generates a
    %% "restart_emulator" instruction.
    {ok, _ , _, []} =
	systools:make_relup(LatestName, [LatestName1], [LatestName1],
			    [{path, P},restart_emulator,silent]),
    ok = check_relup([{db, "2.1"}], [{db, "1.0"}]),
    ok = check_restart_emulator(),


    %% Pre-R15 to Post-R15 upgrade
    {ok, _ , _, Ws} =
	systools:make_relup(LatestName0CurrErts,
			    [LatestName1],
			    [LatestName1],
			    [{path, P},silent]),
    ok = check_relup([{db,"2.1"}], [{db, "1.0"}]),
    ok = check_pre_to_post_r15_restart_emulator(),
    ok = check_pre_to_post_r15_warnings(Ws),


    %% Check that new sasl version generates a restart_new_emulator
    %% instruction
    {ok, _ , _, []} =
	systools:make_relup(CurrentAllFutSaslName,
			    [CurrentAllName],
			    [CurrentAllName],
			    [{path, P},silent]),
    ok = check_relup([{fe, "3.1"}], []),
    ok = check_restart_emulator_diff_coreapp(),


    %% Check that new erts version generates a restart_new_emulator
    %% instruction, if FromSaslVsn >= R15SaslVsn
    %% (One erts_vsn_changed warning for upgrade and one for downgrade)
    {ok, _ , _, [{erts_vsn_changed,_},{erts_vsn_changed,_}]} =
	systools:make_relup(CurrentAllFutErtsName,
			    [CurrentAllName],
			    [CurrentAllName],
			    [{path, P},silent]),
    ok = check_relup([{fe, "3.1"}], []),
    ok = check_restart_emulator_diff_coreapp(),


    %% Check that new erts version generates a restart_new_emulator
    %% instruction, and can be combined with restart_emulator opt.
    %% (One erts_vsn_changed warning for upgrade and one for downgrade)
    {ok, _ , _, [{erts_vsn_changed,_},{erts_vsn_changed,_}]} =
	systools:make_relup(CurrentAllFutErtsName,
			    [CurrentAllName],
			    [CurrentAllName],
			    [{path, P},restart_emulator,silent]),
    ok = check_relup([{fe, "3.1"}], []),
    ok = check_restart_emulator(),
    ok = check_restart_emulator_diff_coreapp(),

    ok = file:set_cwd(OldDir),
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

check_relup_up_only(UpVsnL) ->
    {ok, [{_V1, [{_, _, Up}], []}]} = file:consult(relup),
    [] = foldl(fun(X, Acc) ->
		       true = lists:member(X, Acc),
		       lists:delete(X, Acc) end,
	       UpVsnL,
	       [{App, Vsn} || {load_object_code,{App,Vsn,_}} <- Up]),
    ok.

check_restart_emulator() ->
    {ok, [{_V1, [{_, _, Up}], [{_, _, Dn}]}]} = file:consult(relup),
    restart_emulator = lists:last(Up),
    restart_emulator = lists:last(Dn),
    ok.

check_restart_emulator_up_only() ->
    {ok, [{_V1, [{_, _, Up}], []}]} = file:consult(relup),
    restart_emulator = lists:last(Up),
    ok.

check_restart_emulator_diff_coreapp() ->
    {ok, [{_V1, [{_, _, Up}], [{_, _, Dn}]}]} = file:consult(relup),
    [restart_new_emulator|_] = Up,
    restart_emulator = lists:last(Dn),
    ok.

check_pre_to_post_r15_restart_emulator() ->
    {ok, [{_V1, [{_, _, Up}], [{_, _, Dn}]}]} = file:consult(relup),
    restart_new_emulator = lists:last(Up),
    restart_emulator = lists:last(Dn),
    ok.

check_pre_to_post_r15_warnings(Ws) ->
    true = lists:member(pre_R15_emulator_upgrade,Ws),
    ok.

%% make_relup: Check that appup files may be missing, but only if we
%% don't need them.
no_appup_relup(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir,LatestName}   = create_script(latest_small,Config),
    {_LatestDir0,LatestName0} = create_script(latest_small0,Config),
    {_LatestDir1,LatestName1} = create_script(latest_small1,Config),

    DataDir = filename:absname(?copydir),

    ok = file:set_cwd(LatestDir),

    %% Check that appup might be missing
    P1 = [fname([DataDir, d_no_appup, lib, 'fe-3.1', ebin]),
	  fname([DataDir, lib, kernel, ebin]),
	  fname([DataDir, lib, stdlib, ebin]),
	  fname([DataDir, lib, sasl, ebin])],
    ok =
	systools:make_relup(LatestName, [LatestName], [], [{path, P1}]),
    {ok,_, _, []} =
	systools:make_relup(LatestName, [LatestName], [],
			    [silent, {path, P1}]),

    %% Check that appup might NOT be missing when we need it
    P2 = [fname([DataDir, d_no_appup, lib, 'fe-3.1', ebin]),
	  fname([DataDir, d_no_appup, lib, 'fe-2.1', ebin]),
	  fname([DataDir, lib, kernel, ebin]),
	  fname([DataDir, lib, stdlib, ebin]),
	  fname([DataDir, lib, sasl, ebin])],
    error =
	systools:make_relup(LatestName, [LatestName0], [], [{path, P2}]),
    {error,_,{file_problem, {_,{error,{open,_,_}}}}} =
	systools:make_relup(LatestName, [], [LatestName0],
			    [silent, {path, P2}]),

    %% Check that appups missing vsn traps
    P3 = [fname([DataDir, d_no_appup, lib, 'fe-2.1', ebin]),
	  fname([DataDir, d_no_appup, lib, 'fe-500.18.7', ebin]),
	  fname([DataDir, lib, kernel, ebin]),
	  fname([DataDir, lib, stdlib, ebin]),
	  fname([DataDir, lib, sasl, ebin])],

    error =
	systools:make_relup(LatestName0, [LatestName1], [], [{path, P3}]),
    {error,_,{no_relup, _, _, _}} =
	systools:make_relup(LatestName0, [], [LatestName1],
			    [silent, {path, P3}]),

    ok = file:set_cwd(OldDir),
    ok.

%% make_relup: Check that badly written appup files are detected.
bad_appup_relup(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir,LatestName}   = create_script(latest_small,Config),
    {_LatestDir0,LatestName0} = create_script(latest_small0,Config),

    DataDir = filename:absname(?copydir),
    N2 = [fname([DataDir, d_bad_appup, lib, 'fe-3.1', ebin]),
	  fname([DataDir, d_bad_appup, lib, 'fe-2.1', ebin]),
	  fname([DataDir, lib, kernel, ebin]),
	  fname([DataDir, lib, stdlib, ebin]),
	  fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),

    %% Check that bad appup is trapped
    error =
	systools:make_relup(LatestName, [LatestName0], [], [{path, N2}]),
    {error,_,{file_problem, {_, {error, {parse,_, _}}}}} =
	systools:make_relup(LatestName, [], [LatestName0],
			    [silent, {path, N2}]),

    ok = file:set_cwd(OldDir),
    ok.

%% make_relup: Check some abnormal cases.
abnormal_relup(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir,LatestName}   = create_script(latest0,Config),
    {_LatestDir1,LatestName1} = create_script(latest1,Config),

    %% Check wrong app vsn
    DataDir = filename:absname(?copydir),
    P = [fname([DataDir, d_bad_app_vsn, lib, 'db-2.1', ebin]),
	 fname([DataDir, d_bad_app_vsn, lib, 'fe-3.1', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),

    error = systools:make_relup(LatestName, [LatestName1], [LatestName1],
				[{path, P}]),
    R0 = systools:make_relup(LatestName, [LatestName1], [LatestName1],
			     [silent, {path, P}]),
    {error,systools_make,
     [{error_reading,{db,{no_valid_version,
			  {{"should be","2.1"},
			   {"found file", _, "2.0"}}}}}]} = R0,
    ok = file:set_cwd(OldDir),
    ok.


%% make_relup: Check relup cannot be created is sasl is not in rel file.
no_sasl_relup(Config) when is_list(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {Dir1,Name1} = create_script(latest1_no_sasl,Config),
    {_Dir2,Name2} = create_script(latest1,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(Dir1),

    error = systools:make_relup(Name2, [Name1], [Name1], [{path, P}]),
    R1 = systools:make_relup(Name2, [Name1], [Name1],[silent, {path, P}]),
    {error,systools_relup,{missing_sasl,_}} = R1,

    error = systools:make_relup(Name1, [Name2], [Name2], [{path, P}]),
    R2 = systools:make_relup(Name1, [Name2], [Name2],[silent, {path, P}]),
    {error,systools_relup,{missing_sasl,_}} = R2,

    ok = file:set_cwd(OldDir),
    ok.


%% make_relup: Check that application start type is used in relup
app_start_type_relup(Config) when is_list(Config) ->
    %% This might fail if some applications are not available, if so
    %% skip the test case.
    try create_script(latest_app_start_type2,Config) of
	{Dir2,Name2} ->
	    app_start_type_relup(Dir2,Name2,Config)
    catch throw:{error,Reason} ->
	    {skip,Reason}
    end.

app_start_type_relup(Dir2,Name2,Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Dir1,Name1} = create_script(latest_app_start_type1,Config),

    Release1 = filename:join(Dir1,Name1),
    Release2 = filename:join(Dir2,Name2),

    {ok, Release2Relup, systools_relup, []} = systools:make_relup(Release2, [Release1], [Release1], [{outdir, PrivDir}, silent]),
    {"LATEST_APP_START_TYPE2",
     [{"LATEST_APP_START_TYPE1",[], UpInstructions}],
     [{"LATEST_APP_START_TYPE1",[], DownInstructions}]} = Release2Relup,
    %% ?t:format("Up: ~p",[UpInstructions]),
    %% ?t:format("Dn: ~p",[DownInstructions]),
    [{load_object_code, {mnesia, _, _}},
     {load_object_code, {runtime_tools, _, _}},
     {load_object_code, {snmp, _, _}},
     {load_object_code, {xmerl, _, _}},
     point_of_no_return
     | UpInstructionsT] = UpInstructions,
    true = lists:member({apply,{application,start,[mnesia,permanent]}}, UpInstructionsT),
    true = lists:member({apply,{application,start,[runtime_tools,transient]}}, UpInstructionsT),
    true = lists:member({apply,{application,load,[snmp]}}, UpInstructionsT),
    false = lists:any(fun({apply,{application,_,[xmerl|_]}}) -> true; (_) -> false end, UpInstructionsT),
    [point_of_no_return | DownInstructionsT] = DownInstructions,
    true = lists:member({apply,{application,stop,[mnesia]}}, DownInstructionsT),
    true = lists:member({apply,{application,stop,[runtime_tools]}}, DownInstructionsT),
    true = lists:member({apply,{application,stop,[snmp]}}, DownInstructionsT),
    true = lists:member({apply,{application,stop,[xmerl]}}, DownInstructionsT),
    true = lists:member({apply,{application,unload,[mnesia]}}, DownInstructionsT),
    true = lists:member({apply,{application,unload,[runtime_tools]}}, DownInstructionsT),
    true = lists:member({apply,{application,unload,[snmp]}}, DownInstructionsT),
    true = lists:member({apply,{application,unload,[xmerl]}}, DownInstructionsT),
    ok.


%% make_relup: Check that regexp can be used in .appup for UpFromVsn
%% and DownToVsn.
regexp_relup(Config) ->
    {ok, OldDir} = file:get_cwd(),

    {LatestDir,LatestName}   = create_script(latest_small,Config),
    {_LatestDir0,LatestName0} = create_script(latest_small0,Config),
    {_LatestDir1,LatestName1} = create_script(latest_small2,Config),

    DataDir = filename:absname(?copydir),
    P = [fname([DataDir, d_regexp_appup, lib, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),

    %% Upgrade fe 2.1 -> 3.1, and downgrade 2.1 -> 3.1
    %% Shall match the first entry if fe-3.1 appup.
    {ok, _, _, []} =
	systools:make_relup(LatestName, [LatestName0], [LatestName0],
			    [{path, P}, silent]),
    ok = check_relup([{fe, "3.1"}], [{fe, "2.1"}]),

    %% Upgrade fe 2.1.1 -> 3.1
    %% Shall match the second entry in fe-3.1 appup. Have added a
    %% restart_emulator instruction there to distinguish it from
    %% the first entry...
    {ok, _, _, []} =
	systools:make_relup(LatestName, [LatestName1], [], [{path, P}, silent]),
    ok = check_relup_up_only([{fe, "3.1"}]),
    ok = check_restart_emulator_up_only(),

    %% Attempt downgrade fe 3.1 -> 2.1.1
    %% Shall not match any entry!!
    {error,systools_relup,{no_relup,_,_,_}} =
	systools:make_relup(LatestName, [], [LatestName1], [{path, P}, silent]),

    ok = file:set_cwd(OldDir),

    ok.


%% make_hybrid_boot: Normal case.
%% For upgrade of erts - create a boot file which is a hybrid between
%% old and new release - i.e. starts erts, kernel, stdlib, sasl from
%% new release, all other apps from old release.
normal_hybrid(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {Dir1,Name1} = create_script(latest1,Config),
    {_Dir2,Name2} = create_script(current_all,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(Dir1),

    {ok, _ , []} = systools:make_script(Name1,[{path, P},silent]),
    {ok, _ , []} = systools:make_script(Name2,[{path, P},silent]),
    {ok,Boot1} = file:read_file(Name1 ++ ".boot"),
    {ok,Boot2} = file:read_file(Name2 ++ ".boot"),

    ok = file:set_cwd(OldDir),

    {ok,Hybrid} = systools_make:make_hybrid_boot("tmp_vsn",Boot1,Boot2,
                                                 [dummy,args]),

    {script,{"Test release","tmp_vsn"},Script} = binary_to_term(Hybrid),
    ct:log("~p.~n",[Script]),

    %% Check that all paths to base apps are replaced by paths from BaseLib
    Boot1Str = io_lib:format("~p~n",[binary_to_term(Boot1)]),
    Boot2Str = io_lib:format("~p~n",[binary_to_term(Boot2)]),
    HybridStr = io_lib:format("~p~n",[binary_to_term(Hybrid)]),
    ReOpts = [global,{capture,first,list},unicode],
    {match,OldKernelMatch} = re:run(Boot1Str,"kernel-[0-9\.]+",ReOpts),
    {match,OldStdlibMatch} = re:run(Boot1Str,"stdlib-[0-9\.]+",ReOpts),
    {match,OldSaslMatch} = re:run(Boot1Str,"sasl-[0-9\.]+",ReOpts),

    {match,NewKernelMatch} = re:run(Boot2Str,"kernel-[0-9\.]+",ReOpts),
    {match,NewStdlibMatch} = re:run(Boot2Str,"stdlib-[0-9\.]+",ReOpts),
    {match,NewSaslMatch} = re:run(Boot2Str,"sasl-[0-9\.]+",ReOpts),

    {match,NewKernelMatch} = re:run(HybridStr,"kernel-[0-9\.]+",ReOpts),
    {match,NewStdlibMatch} = re:run(HybridStr,"stdlib-[0-9\.]+",ReOpts),
    {match,NewSaslMatch} = re:run(HybridStr,"sasl-[0-9\.]+",ReOpts),

    NewKernelN = length(NewKernelMatch),
    NewKernelN = length(OldKernelMatch),
    NewStdlibN = length(NewStdlibMatch),
    NewStdlibN = length(OldStdlibMatch),
    NewSaslN = length(NewSaslMatch),
    NewSaslN = length(OldSaslMatch),

    %% Check that kernelProcesses are taken from new boot script
    {script,_,Script2} = binary_to_term(Boot2),
    NewKernelProcs = [KP || KP={kernelProcess,_,_} <- Script2],
    NewKernelProcs = [KP || KP={kernelProcess,_,_} <- Script],

    %% Check that application load instruction has correct versions
    Apps = application:loaded_applications(),
    {_,_,KernelVsn} = lists:keyfind(kernel,1,Apps),
    {_,_,StdlibVsn} = lists:keyfind(stdlib,1,Apps),
    {_,_,SaslVsn} = lists:keyfind(sasl,1,Apps),

    [KernelInfo] = [I || {kernelProcess,application_controller,
			  {application_controller,start,
			   [{application,kernel,I}]}} <- Script],
    [StdlibInfo] = [I || {apply,
			  {application,load,
			   [{application,stdlib,I}]}} <- Script],
    [SaslInfo] = [I || {apply,
			{application,load,
			 [{application,sasl,I}]}} <- Script],

    {vsn,KernelVsn} = lists:keyfind(vsn,1,KernelInfo),
    {vsn,StdlibVsn} = lists:keyfind(vsn,1,StdlibInfo),
    {vsn,SaslVsn} = lists:keyfind(vsn,1,SaslInfo),

    %% Check that new_emulator_upgrade call is added
    [_,{apply,{release_handler,new_emulator_upgrade,[dummy,args]}}|_] =
	lists:reverse(Script),

    %% Check that db-1.0 and fe-3.1 are used (i.e. vsns from old release)
    %% And that fe is in there (it exists in old rel but not in new)
    {match,DbMatch} = re:run(HybridStr,"db-[0-9\.]+",ReOpts),
    {match,[_|_]=FeMatch} = re:run(HybridStr,"fe-[0-9\.]+",ReOpts),
    true = lists:all(fun(["db-1.0"]) -> true;
			(_)  -> false
		     end,
		     DbMatch),
    true = lists:all(fun(["fe-3.1"]) -> true;
			(_)  -> false
		     end,
		     FeMatch),

    %% Check that script has same length as old script, plus one (the
    %% new_emulator_upgrade apply)
    {_,_,Old} = binary_to_term(Boot1),
    OldLength = length(Old),
    NewLength = length(Script),
    NewLength = OldLength + 1,

    ok.

%% make_hybrid_boot: No sasl in from-release.
%% Check that systools_make:make_hybrid_boot fails with a meaningful
%% error message if the FromBoot does not include the sasl
%% application.
hybrid_no_old_sasl(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {Dir1,Name1} = create_script(latest1_no_sasl,Config),
    {_Dir2,Name2} = create_script(current_all,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(Dir1),

    {ok, _ , [{warning,missing_sasl}]} =
	systools:make_script(Name1,[{path, P},silent]),
    {ok, _ , []} = systools:make_script(Name2,[{path, P},silent]),
    {ok,Boot1} = file:read_file(Name1 ++ ".boot"),
    {ok,Boot2} = file:read_file(Name2 ++ ".boot"),

    {error,{app_not_replaced,sasl}} =
	systools_make:make_hybrid_boot("tmp_vsn",Boot1,Boot2,[dummy,args]),

    ok = file:set_cwd(OldDir),
    ok.


%% make_hybrid_boot: No sasl in to-release.
%% Check that systools_make:make_hybrid_boot fails with a meaningful
%% error message if the ToBoot does not include the sasl
%% application.
hybrid_no_new_sasl(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {Dir1,Name1} = create_script(latest1,Config),
    {_Dir2,Name2} = create_script(current_all_no_sasl,Config),

    DataDir = filename:absname(?copydir),
    LibDir = [fname([DataDir, d_normal, lib])],
    P = [fname([LibDir, '*', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(Dir1),

    {ok, _ , []} = systools:make_script(Name1,[{path, P},silent]),
    {ok, _ , [{warning,missing_sasl}]} =
	systools:make_script(Name2,[{path, P},silent]),
    {ok,Boot1} = file:read_file(Name1 ++ ".boot"),
    {ok,Boot2} = file:read_file(Name2 ++ ".boot"),

    {error,{app_not_found,sasl}} =
	systools_make:make_hybrid_boot("tmp_vsn",Boot1,Boot2,[dummy,args]),

    ok = file:set_cwd(OldDir),
    ok.



%% options: {outdir,Dir} option
otp_6226_outdir(Config) when is_list(Config) ->
    PrivDir = ?privdir,
    {ok, OldDir} = file:get_cwd(),

    {LatestDir, LatestName} = create_script(latest0,Config),
    {_LatestDir, LatestName1} = create_script(latest1,Config),

    DataDir = filename:absname(?copydir),
    LibDir = fname([DataDir, d_normal, lib]),
    P = [fname([LibDir, 'db-2.1', ebin]),
	 fname([LibDir, 'db-1.0', ebin]),
	 fname([LibDir, 'fe-3.1', ebin]),
	 fname([DataDir, lib, kernel, ebin]),
	 fname([DataDir, lib, stdlib, ebin]),
	 fname([DataDir, lib, sasl, ebin])],

    ok = file:set_cwd(LatestDir),


    %% Create an outdir1 directory
    ok = file:make_dir("outdir1"),

    %% ==== Now test systools:make_script ====
    %% a) badarg
    {'EXIT', {{badarg,[{outdir,outdir1}]}, _}} =
	(catch systools:make_script(LatestName, [{outdir,outdir1},
						 {path,P},silent])),

    %% b) absolute path
    Outdir1 = filename:join(PrivDir, "outdir1"),
    {ok,_,[]} = systools:make_script(LatestName, [{outdir,Outdir1},
						  {path,P},silent]),
    Script1 = filename:join(Outdir1, LatestName ++ ".script"),
    Boot1 = filename:join(Outdir1, LatestName ++ ".boot"),
    true = filelib:is_file(Script1),
    true = filelib:is_file(Boot1),
    ok = file:delete(Script1),
    ok = file:delete(Boot1),

    %% c) relative path
    {ok,_,[]} = systools:make_script(LatestName, [{outdir,"./outdir1"},
						  {path,P},silent]),
    true = filelib:is_file(Script1),
    true = filelib:is_file(Boot1),
    ok = file:delete(Script1),
    ok = file:delete(Boot1),

    %% d) absolute but incorrect path
    Outdir2 = filename:join(PrivDir, "outdir2"),
    Script2 = filename:join(Outdir2, LatestName ++ ".script"),
    {error,_,{open,Script2,_}} =
	systools:make_script(LatestName, [{outdir,Outdir2},{path,P},silent]),

    %% e) relative but incorrect path
    {error,_,{open,_,_}} =
	systools:make_script(LatestName, [{outdir,"./outdir2"},{path,P},silent]),

    %% f) with .rel in another directory than cwd
    ok = file:set_cwd(Outdir1),
    {ok,_,[]} = systools:make_script(filename:join(PrivDir, LatestName),
				     [{outdir,"."},{path,P},silent]),
    true = filelib:is_file(LatestName ++ ".script"),
    true = filelib:is_file(LatestName ++ ".boot"),
    ok = file:delete(LatestName ++ ".script"),
    ok = file:delete(LatestName ++ ".boot"),
    ok = file:set_cwd(LatestDir),

    %% ==== Now test systools:make_tar =====
    {ok,_,[]} = systools:make_script(LatestName, [{path,P},silent]),
    %% a) badarg
    {'EXIT', {{badarg, [{outdir,outdir1}]}, _}} =
    	(catch systools:make_tar(LatestName,[{outdir,outdir1},{path,P},silent])),

    %% b) absolute path
    {ok,_,[]} = systools:make_tar(LatestName, [{outdir,Outdir1},
					       {path,P},silent]),
    Tar1 = filename:join(Outdir1,LatestName++".tar.gz"),
    true = filelib:is_file(Tar1),
    ok = file:delete(Tar1),

    %% c) relative path
    {ok,_,[]} = systools:make_tar(LatestName, [{outdir,"./outdir1"},
					       {path,P},silent]),
    true = filelib:is_file(Tar1),
    ok = file:delete(Tar1),

    %% d) absolute but incorrect path
    Tar2 = filename:join(Outdir2,LatestName++".tar.gz"),
    {error,_,{tar_error,{open,Tar2,{Tar2,enoent}}}} =
	systools:make_tar(LatestName, [{outdir,Outdir2},{path,P},silent]),

    %% e) relative but incorrect path
    {error,_,{tar_error,{open,_,_}}} =
	systools:make_tar(LatestName, [{outdir,"./outdir2"},{path,P},silent]),

    %% f) with .rel in another directory than cwd
    ok = file:set_cwd(Outdir1),
    {ok,_,[]} = systools:make_tar(filename:join(PrivDir, LatestName),
				  [{outdir,"."},{path,P},silent]),
    true = filelib:is_file(Tar1),
    ok = file:set_cwd(LatestDir),

    %% ===== Now test systools:make_relup =====
    %% a) badarg
    {'EXIT', {{badarg, [{outdir,outdir1}]}, _}} =
    	(catch systools:make_relup(LatestName,[LatestName1],[LatestName1],
    				   [{outdir,outdir1}, 
				    {path,P},silent])),

    %% b) absolute path
    Relup = filename:join(Outdir1, "relup"),
    {ok,_,_,[]} = systools:make_relup(LatestName,[LatestName1],[LatestName1],
				      [{outdir,Outdir1},
				       {path,P},silent]),
    true = filelib:is_file(Relup),
    ok = file:delete(Relup),

    %% c) relative path
    {ok,_,_,[]} = systools:make_relup(LatestName,[LatestName1],[LatestName1],
				      [{outdir,"./outdir1"},
				       {path,P},silent]),
    true = filelib:is_file(Relup),
    ok = file:delete(Relup),

    %% d) absolute but incorrect path
    {error,_,{file_problem,{"relup",{open,enoent}}}} =
	systools:make_relup(LatestName,[LatestName1],[LatestName1],
			    [{outdir,Outdir2},{path,P},silent]),

    %% e) relative but incorrect path
    {error,_,{file_problem,{"relup",{open,enoent}}}} =
	systools:make_relup(LatestName,[LatestName1],[LatestName1],
			    [{outdir,"./outdir2"},{path,P},silent]),

    %% f) with .rel in another directory than cwd
    %% -- not necessary to test since relup by default is placed in
    %%    cwd, not in the same directory as the .rel file --

    %% Change back to previous working directory
    ok = file:set_cwd(OldDir),
    ok.


%% Test that all default values can be used as values in the .app file
app_file_defaults(Config) ->
    PrivDir = ?config(priv_dir,Config),
    Name = app1,
    NameStr = atom_to_list(Name),
    Vsn = "1.0",
    AppSpec = app_spec(Name,#{vsn=>"1.0"}),
    ok = file:write_file(filename:join(PrivDir,NameStr ++ ".app"),
			 io_lib:format("~p.~n",[AppSpec])),
    {ok,_} = systools_make:read_application(NameStr,Vsn,[PrivDir],[]),
    ok.

app_spec(Name,New) ->
    {application,Name,app_spec(New)}.

app_spec(New) ->
    Default = #{description => "",
		id => "",
		vsn => "",
		modules => [],
		maxP => infinity,
		maxT => infinity,
		registered => [],
		included_applications => [],
		applications => [],
		env => [],
		mod => [],
		start_phases => undefined,
		runtime_dependencies => []},
    maps:to_list(maps:merge(Default,New)).

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
    Apps = core_apps(current) ++ [{db,"2.1"},{fe,"3.1"}],
    do_create_script(latest,Config,"4.4",Apps);
create_script(latest_no_mod_vsn,Config) ->
    Apps = core_apps(current) ++ [{db,"3.1"},{fe,"3.1"}],
    do_create_script(latest_no_mod_vsn,Config,"4.4",Apps);
create_script(latest0,Config) ->
    Apps = core_apps("1.0") ++ [{db,"2.1"},{fe,"3.1"}],
    do_create_script(latest0,Config,"4.4",Apps);
create_script(latest0_current_erts,Config) ->
    Apps = core_apps("1.0") ++ [{db,"2.1"},{fe,"3.1"}],
    do_create_script(latest0_current_erts,Config,current,Apps);
create_script(latest1,Config) ->
    Apps = core_apps("1.0") ++ [{db,"1.0"},{fe,"3.1"}],
    do_create_script(latest1,Config,"4.4",Apps);
create_script(latest1_no_sasl,Config) ->
    Apps = [{kernel,"1.0"},{stdlib,"1.0"},{db,"1.0"},{fe,"3.1"}],
    do_create_script(latest1_no_sasl,Config,"4.4",Apps);
create_script(latest2,Config) ->
    Apps = core_apps("1.0") ++ [{db,"1.0"},{fe,"2.1"}],
    do_create_script(latest2,Config,"4.3",Apps);
create_script(latest_small,Config) ->
    Apps = core_apps("1.0") ++ [{fe,"3.1"}],
    do_create_script(latest_small,Config,"4.4",Apps);
create_script(latest_small0,Config) ->		%Differs in fe vsn
    Apps = core_apps("1.0") ++ [{fe,"2.1"}],
    do_create_script(latest_small0,Config,"4.4",Apps);
create_script(latest_small1,Config) ->
    Apps = core_apps("1.0") ++ [{fe,"500.18.7"}],
    do_create_script(latest_small1,Config,"4.4",Apps);
create_script(latest_small2,Config) ->
    Apps = core_apps("1.0") ++ [{fe,"2.1.1"}],
    do_create_script(latest_small2,Config,"4.4",Apps);
create_script(latest_nokernel,Config) ->
    Apps = [{db,"2.1"},{fe,"3.1"}],
    do_create_script(latest_nokernel,Config,"4.4",Apps);
create_script(latest_kernel_start_type,Config) ->
    Apps = [{kernel,"1.0",load},{stdlib,"1.0"},{db,"2.1"},{fe,"3.1"}],
    do_create_script(latest_kernel_start_type,Config,"4.4",Apps);
create_script(latest_stdlib_start_type,Config) ->
    Apps = [{kernel,"1.0"},{stdlib,"1.0",load},{db,"2.1"},{fe,"3.1"}],
    do_create_script(latest_stdlib_start_type,Config,"4.4",Apps);
create_script(latest_no_stdlib,Config) ->
    Apps = [{kernel,"1.0"},{db,"2.1"},{fe,"3.1"}],
    do_create_script(latest_no_stdlib,Config,"4.4",Apps);
create_script(latest_app_start_type1,Config) ->
    Apps = core_apps(current),
    do_create_script(latest_app_start_type1,Config,current,Apps);
create_script(latest_app_start_type2,Config) ->
    OtherApps = [{mnesia,current,permanent},
		 {runtime_tools,current,transient},
		 {snmp,current,load},
		 {xmerl,current,none}],
    Apps = core_apps(current) ++ OtherApps,
    do_create_script(latest_app_start_type2,Config,current,Apps);
create_script(current_all_no_sasl,Config) ->
    Apps = [{kernel,current},{stdlib,current},{db,"2.1"},{fe,"3.1"}],
    do_create_script(current_all_no_sasl,Config,current,Apps);
create_script(current_all,Config) ->
    Apps = core_apps(current) ++ [{db,"2.1"}],
    do_create_script(current_all,Config,current,Apps);
create_script(current_all_future_erts,Config) ->
    Apps = core_apps(current) ++ [{db,"2.1"},{fe,"3.1"}],
    do_create_script(current_all_future_erts,Config,"99.99",Apps);
create_script(current_all_future_sasl,Config) ->
    Apps = [{kernel,current},{stdlib,current},{sasl,"9.9"},{db,"2.1"},{fe,"3.1"}],
    do_create_script(current_all_future_sasl,Config,current,Apps);
create_script({unicode,RelVsn},Config) ->
    Apps = core_apps(current) ++ [{ua,"1.0"}],
    do_create_script(unicode,RelVsn,Config,current,Apps);
create_script(duplicate_modules,Config) ->
    Apps = core_apps(current) ++ [{app1,"1.0"},{app2,"1.0"}],
    do_create_script(duplicate_modules,Config,current,Apps).


do_create_script(Id,Config,ErtsVsn,AppVsns) ->
    do_create_script(Id,string:to_upper(atom_to_list(Id)),Config,ErtsVsn,AppVsns).
do_create_script(Id,RelVsn,Config,ErtsVsn,AppVsns) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, Id),
    {ok,Fd} = file:open(Name++".rel",write),
    RelfileContent =
	{release,{"Test release", RelVsn},
	 {erts,erts_vsn(ErtsVsn)},
	 app_vsns(AppVsns)},
    io:format(Fd,"~p.~n",[RelfileContent]),
    ok = file:close(Fd),
    {filename:dirname(Name), filename:basename(Name)}.

core_apps(Vsn) ->
    [{App,Vsn} || App <- [kernel,stdlib,sasl]].

app_vsns(AppVsns) ->
    [{App,app_vsn(App,Vsn)} || {App,Vsn} <- AppVsns] ++
	[{App,app_vsn(App,Vsn),Type} || {App,Vsn,Type} <- AppVsns].
app_vsn(App,current) ->
    case application:load(App) of
	Ok when Ok==ok; Ok=={error,{already_loaded,App}} ->
	    {ok,Vsn} = application:get_key(App,vsn),
	    Vsn;
	Error ->
	    throw(Error)
    end;
app_vsn(_App,Vsn) ->
    Vsn.

erts_vsn(current) -> erlang:system_info(version);
erts_vsn(Vsn) -> Vsn.


create_include_files(inc1, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc1),
    create_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {t6, \"1.0\"}, {t5, \"1.0\"}, \n"
	"  {t4, \"1.0\"}, {t3, \"1.0\"}, {t2, \"1.0\"}, \n"
	"  {t1, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc2, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc2),
    create_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

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
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc3),
    create_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

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
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc4),
    create_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

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
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc5),
    create_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

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
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc6),
    create_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

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
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc7),
    create_apps(PrivDir),
    create_app(t7, PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

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
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc8),
    create_circular_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t8 uses t9 and t10 includes t9 !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {t8, \"1.0\"}, {t9, \"1.0\"}, {t10, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc9, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc9),
    create_circular_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t8 uses t9, t9 uses t10 and t10 includes t8 ==> circular !!
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {t8, \"1.0\"}, {t9, \"1.0\"}, {t10, \"1.0\", [t8]}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc10, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc10),
    create_circular_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    %% t9 tries to include not specified (in .app file) application !
    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {t8, \"1.0\"}, {t9, \"1.0\", [t7]}, {t10, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(inc11, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, inc11),
    create_apps2(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {t11, \"1.0\"}, \n"
	"  {t12, \"1.0\"}, \n"
	"  {t13, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(otp_3065_circular_dependenies, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, otp_3065_circular_dependenies),
    create_apps_3065(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {chAts, \"1.0\"}, {aa12, \"1.0\"}, \n"
	"  {chTraffic, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(sort_apps, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, sort_apps),
    create_sort_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {t14, \"1.0\"}, \n"
	"  {t20, \"1.0\"}, \n"
	"  {t19, \"1.0\"}, \n"
	"  {t18, \"1.0\"}, \n"
	"  {t17, \"1.0\"}, \n"
	"  {t16, \"1.0\"}, \n"
	"  {t15, \"1.0\"}]}.\n",
    file:write_file(Name ++ ".rel", list_to_binary(Rel)),
    {filename:dirname(Name), filename:basename(Name)};

create_include_files(sort_apps_rev, Config) ->
    PrivDir = ?privdir,
    Name = fname(PrivDir, sort_apps_rev),
    create_sort_apps(PrivDir),

    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),

    Rel = "{release, {\"test\",\"R1A\"}, {erts, \"45\"},\n"
	" [{kernel, \"" ++ KernelVer ++ "\"}, {stdlib, \""
	++ StdlibVer ++ "\"},\n"
	"  {t14, \"1.0\"}, \n"
	"  {t18, \"1.0\"}, \n"
	"  {t19, \"1.0\"}, \n"
	"  {t20, \"1.0\"}, \n"
	"  {t15, \"1.0\"}, \n"
	"  {t16, \"1.0\"}, \n"
	"  {t17, \"1.0\"}]}.\n",
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

create_sort_apps(Dir) ->
    T14 = "{application, t14,\n"
	" [{vsn, \"1.0\"},\n"
	"  {description, \"test\"},\n"
	"  {modules, []},\n"
	"  {applications, [t18,t20,t19]},\n"
	"  {included_applications, [t15,t17,t16]},\n"
	"  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't14.app'), list_to_binary(T14)),

    T15 = "{application, t15,\n"
	" [{vsn, \"1.0\"},\n"
	"  {description, \"test\"},\n"
	"  {modules, []},\n"
	"  {applications, []},\n"
	"  {included_applications, []},\n"
	"  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't15.app'), list_to_binary(T15)),

    T16 = "{application, t16,\n"
	" [{vsn, \"1.0\"},\n"
	"  {description, \"test\"},\n"
	"  {modules, []},\n"
	"  {applications, []},\n"
	"  {included_applications, []},\n"
	"  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't16.app'), list_to_binary(T16)),

    T17 = "{application, t17,\n"
	" [{vsn, \"1.0\"},\n"
	"  {description, \"test\"},\n"
	"  {modules, []},\n"
	"  {applications, []},\n"
	"  {included_applications, []},\n"
	"  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't17.app'), list_to_binary(T17)),

    T18 = "{application, t18,\n"
	" [{vsn, \"1.0\"},\n"
	"  {description, \"test\"},\n"
	"  {modules, []},\n"
	"  {applications, []},\n"
	"  {included_applications, []},\n"
	"  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't18.app'), list_to_binary(T18)),

    T19 = "{application, t19,\n"
	" [{vsn, \"1.0\"},\n"
	"  {description, \"test\"},\n"
	"  {modules, []},\n"
	"  {applications, []},\n"
	"  {included_applications, []},\n"
	"  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't19.app'), list_to_binary(T19)),

    T20 = "{application, t20,\n"
	" [{vsn, \"1.0\"},\n"
	"  {description, \"test\"},\n"
	"  {modules, []},\n"
	"  {applications, []},\n"
	"  {included_applications, []},\n"
	"  {registered, []}]}.\n",
    file:write_file(fname(Dir, 't20.app'), list_to_binary(T20)).

fname(N) ->
    filename:join(N).

fname(Dir, Basename) ->
    filename:join(Dir, Basename).
