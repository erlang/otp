%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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

-module(escript_SUITE).
-export([
	all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 basic/1,
	 errors/1,
	 strange_name/1,
	 emulator_flags/1,
	 module_script/1,
	 beam_script/1,
	 archive_script/1,
	 archive_script_file_access/1,
	 epp/1,
	 create_and_extract/1,
	 foldl/1,
	 overflow/1,
	 verify_sections/3
	]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, errors, strange_name, emulator_flags,
     module_script, beam_script, archive_script, epp,
     create_and_extract, foldl, overflow,
     archive_script_file_access].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?t:minutes(2)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic(Config) when is_list(Config) ->
    Data = ?config(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    ?line run(Dir, "factorial 5",
	      <<"factorial 5 = 120\nExitCode:0">>),
    ?line run(Dir, "factorial_compile 10",
	      <<"factorial 10 = 3628800\nExitCode:0">>),
    ?line run(Dir, "factorial_compile_main 7",
	      <<"factorial 7 = 5040\nExitCode:0">>),
    ?line run(Dir, "factorial_warning 20",
	      [data_dir,<<"factorial_warning:12: Warning: function bar/0 is unused\n"
			 "factorial 20 = 2432902008176640000\nExitCode:0">>]),
    ?line run_with_opts(Dir, "-s", "factorial_warning",
	      [data_dir,<<"factorial_warning:12: Warning: function bar/0 is unused\nExitCode:0">>]),
    ?line run_with_opts(Dir, "-s -i", "factorial_warning",
	      [data_dir,<<"factorial_warning:12: Warning: function bar/0 is unused\nExitCode:0">>]),
    ?line run_with_opts(Dir, "-c -s", "factorial_warning",
	      [data_dir,<<"factorial_warning:12: Warning: function bar/0 is unused\nExitCode:0">>]),
    ?line run(Dir, "filesize "++filename:join(?config(data_dir, Config),"filesize"),
	      [data_dir,<<"filesize:11: Warning: function id/1 is unused\n324\nExitCode:0">>]),
    ?line run(Dir, "test_script_name",
	      [data_dir,<<"test_script_name\nExitCode:0">>]),
    ?line run(Dir, "tail_rec 1000",
	      [<<"ok\nExitCode:0">>]),

    %% We expect the trap_exit flag for the process to be false,
    %% since that is the default state for newly spawned processes.
    ?line run(Dir, "trap_exit",
	      <<"false\nExitCode:0">>),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

errors(Config) when is_list(Config) ->
    Data = ?config(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    ?line run(Dir, "compile_error",
	      [data_dir,<<"compile_error:5: syntax error before: '*'\n">>,
	       data_dir,<<"compile_error:8: syntax error before: blarf\n">>,
	       <<"escript: There were compilation errors.\nExitCode:127">>]),
    ?line run(Dir, "lint_error",
	      [data_dir,<<"lint_error:6: function main/1 already defined\n">>,
	       data_dir,"lint_error:8: variable 'ExitCode' is unbound\n",
	       <<"escript: There were compilation errors.\nExitCode:127">>]),
    ?line run_with_opts(Dir, "-s", "lint_error",
	      [data_dir,<<"lint_error:6: function main/1 already defined\n">>,
	       data_dir,"lint_error:8: variable 'ExitCode' is unbound\n",
	       <<"escript: There were compilation errors.\nExitCode:127">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strange_name(Config) when is_list(Config) ->
    Data = ?config(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    ?line run(Dir, "strange.name -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emulator_flags(Config) when is_list(Config) ->
    Data = ?config(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    ?line run(Dir, "emulator_flags -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[{nostick,[]}]\n"
		"mnesia:[{mnesia,[\"dir\",\"a/directory\"]},{mnesia,[\"debug\",\"verbose\"]}]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pick the source code from the emulator_flags script
%% Generate a new escript with a module header

module_script(Config) when is_list(Config) ->
    %% Read orig file
    Data = ?config(data_dir, Config),
    OrigFile = filename:join([Data,"emulator_flags"]),
    {ok, OrigBin} = file:read_file(OrigFile),
    ?line [Shebang, Mode, Flags | Source] = string:tokens(binary_to_list(OrigBin), "\n"),
    ?line {ok, OrigFI} = file:read_file_info(OrigFile),

    %% Write source file
    Priv = ?config(priv_dir, Config),
    Dir = filename:absname(Priv), % Get rid of trailing slash.
    Base = "module_script",
    ErlFile = filename:join([Priv, Base ++ ".erl"]),
    ErlCode = ["\n-module(", Base, ").\n",
	       "-export([main/1]).\n\n",
	       string:join(Source, "\n"),
	       "\n"],
    ?line ok = file:write_file(ErlFile, ErlCode),

    %%%%%%%
    %% Create and run scripts without emulator flags

    %% With shebang
    NoArgsBase = Base ++ "_no_args_with_shebang",
    NoArgsFile = filename:join([Priv, NoArgsBase]),
    ?line ok = file:write_file(NoArgsFile,
			       [Shebang, "\n",
				ErlCode]),
    ?line ok = file:write_file_info(NoArgsFile, OrigFI),

    ?line run(Dir, NoArgsBase ++ " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    ?line run_with_opts(Dir, "", NoArgsBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    %% Without shebang
    NoArgsBase2 = Base ++ "_no_args_without_shebang",
    NoArgsFile2 = filename:join([Priv, NoArgsBase2]),
    ?line ok = file:write_file(NoArgsFile2,
			       ["Something else than shebang!!!", "\n",
				ErlCode]),
    ?line ok = file:write_file_info(NoArgsFile2, OrigFI),

    ?line run_with_opts(Dir, "", NoArgsBase2 ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    %% Plain module without header
    NoArgsBase3 = Base ++ "_no_args_without_header",
    NoArgsFile3 = filename:join([Priv, NoArgsBase3]),
    ?line ok = file:write_file(NoArgsFile3, [ErlCode]),
    ?line ok = file:write_file_info(NoArgsFile3, OrigFI),

    ?line run_with_opts(Dir, "", NoArgsBase3 ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    %%%%%%%
    %% Create and run scripts with emulator flags

    %% With shebang
    ArgsBase = Base ++ "_args_with_shebang",
    ArgsFile = filename:join([Priv, ArgsBase]),
    ?line ok = file:write_file(ArgsFile,
			       [Shebang, "\n",
				Mode, "\n",
				Flags, "\n",
				ErlCode]),
    ?line ok = file:write_file_info(ArgsFile, OrigFI),

    ?line run(Dir, ArgsBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[{nostick,[]}]\n"
		"mnesia:[{mnesia,[\"dir\",\"a/directory\"]},{mnesia,[\"debug\",\"verbose\"]}]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pick the source code from the emulator_flags script and compile it.
%% Generate a new escript containing the beam code and the escript header
beam_script(Config) when is_list(Config) ->
    %% Read orig file
    Data = ?config(data_dir, Config),
    OrigFile = filename:join([Data,"emulator_flags"]),
    {ok, OrigBin} = file:read_file(OrigFile),
    ?line [Shebang, Mode, Flags | Source] = string:tokens(binary_to_list(OrigBin), "\n"),
    ?line {ok, OrigFI} = file:read_file_info(OrigFile),

    %% Write source file
    Priv = ?config(priv_dir, Config),
    Dir = filename:absname(Priv), % Get rid of trailing slash.
    Base = "beam_script",
    ErlFile = filename:join([Priv, Base ++ ".erl"]),
    ?line ok = file:write_file(ErlFile,
			       ["\n-module(", Base, ").\n",
				"-export([main/1]).\n\n",
				string:join(Source, "\n"),
				"\n"]),

    %% Compile the code
    ?line {ok, _Mod, BeamCode} = compile:file(ErlFile, [binary]),

    %%%%%%%
    %% Create and run scripts without emulator flags

    %% With shebang
    NoArgsBase = Base ++ "_no_args_with_shebang",
    NoArgsFile = filename:join([Priv, NoArgsBase]),
    ?line ok = file:write_file(NoArgsFile,
			       [Shebang, "\n",
				BeamCode]),
    ?line ok = file:write_file_info(NoArgsFile, OrigFI),

    ?line run(Dir, NoArgsBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    ?line run_with_opts(Dir, "", NoArgsBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    %% Without shebang
    NoArgsBase2 = Base ++ "_no_args_without_shebang",
    NoArgsFile2 = filename:join([Priv, NoArgsBase2]),
    ?line ok = file:write_file(NoArgsFile2,
			       ["Something else than shebang!!!", "\n",
				BeamCode]),
    ?line ok = file:write_file_info(NoArgsFile2, OrigFI),

    ?line run_with_opts(Dir, "", NoArgsBase2 ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    %% Plain beam file without header
    NoArgsBase3 = Base ++ "_no_args_without_header",
    NoArgsFile3 = filename:join([Priv, NoArgsBase3]),
    ?line ok = file:write_file(NoArgsFile3, [BeamCode]),
    ?line ok = file:write_file_info(NoArgsFile3, OrigFI),

    ?line run_with_opts(Dir, "", NoArgsBase3 ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    %%%%%%%
    %% Create and run scripts with emulator flags

    %% With shebang
    ArgsBase = Base ++ "_args",
    ArgsFile = filename:join([Priv, ArgsBase]),
    ?line ok = file:write_file(ArgsFile,
			       [Shebang, "\n",
				Mode, "\n",
				Flags, "\n",
				BeamCode]),
    ?line ok = file:write_file_info(ArgsFile, OrigFI),

    ?line run(Dir, ArgsBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[{nostick,[]}]\n"
		"mnesia:[{mnesia,[\"dir\",\"a/directory\"]},{mnesia,[\"debug\",\"verbose\"]}]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create an archive file containing two entire applications plus two
%% alternate main modules. Generate a new escript containing the archive
%% (with .app and .beam files and) and the escript header.

archive_script(Config) when is_list(Config) ->
    %% Copy the orig files to priv_dir
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Archive = filename:join([PrivDir, "archive_script.zip"]),
    ?line {ok, _} = zip:create(Archive, ["archive_script"],
			       [{compress, []}, {cwd, DataDir}]),
    ?line {ok, _} = zip:extract(Archive, [{cwd, PrivDir}]),
    TopDir = filename:join([PrivDir, "archive_script"]),

    %% Compile the code
    ?line ok = compile_app(TopDir, "archive_script_dict"),
    ?line ok = compile_app(TopDir, "archive_script_dummy"),
    ?line {ok, MainFiles} = file:list_dir(TopDir),
    ?line ok = compile_files(MainFiles, TopDir, TopDir),

    %% Create the archive
    {ok, TopFiles} = file:list_dir(TopDir),
    ?line {ok, {_, ArchiveBin}} = zip:create(Archive, TopFiles,
					     [memory, {compress, []}, {cwd, TopDir}]),

    %% Read the source script
    OrigFile = filename:join([DataDir, "emulator_flags"]),
    {ok, OrigBin} = file:read_file(OrigFile),
    ?line [Shebang, Mode, _Flags | _Source] =
	string:tokens(binary_to_list(OrigBin), "\n"),
    Flags = "%%! -archive_script_dict foo bar"
	" -archive_script_dict foo"
	" -archive_script_dummy bar",
    ?line {ok, OrigFI} = file:read_file_info(OrigFile),

    %%%%%%%
    %% Create and run scripts without emulator flags
    MainBase = "archive_script_main",
    MainScript = filename:join([PrivDir, MainBase]),

    %% With shebang
    ?line ok = file:write_file(MainScript,
			       [Shebang, "\n",
				Flags, "\n",
				ArchiveBin]),
    ?line ok = file:write_file_info(MainScript, OrigFI),

    ?line run(PrivDir, MainBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
		"dummy:[{archive_script_dummy,[\"bar\"]}]\n"
		"priv:{ok,<<\"Some private data...\\n\">>}\n"
		"ExitCode:0">>]),

    ?line run_with_opts(PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
		"dummy:[{archive_script_dummy,[\"bar\"]}]\n"
		"priv:{ok,<<\"Some private data...\\n\">>}\n"
		"ExitCode:0">>]),

    ?line ok = file:rename(MainScript, MainScript ++ "_with_shebang"),

    %% Without shebang (no flags)
    ?line ok = file:write_file(MainScript,
			       ["Something else than shebang!!!", "\n",
				ArchiveBin]),
    ?line ok = file:write_file_info(MainScript, OrigFI),

    ?line run_with_opts(PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"dict:[]\n"
		"dummy:[]\n"
		"priv:{ok,<<\"Some private data...\\n\">>}\n"
		"ExitCode:0">>]),
    ?line ok = file:rename(MainScript, MainScript ++ "_without_shebang"),

    %% Plain archive without header (no flags)

    ?line ok = file:write_file(MainScript, [ArchiveBin]),
    ?line ok = file:write_file_info(MainScript, OrigFI),

    ?line run_with_opts(PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"dict:[]\n"
		"dummy:[]\n"
		"priv:{ok,<<\"Some private data...\\n\">>}\n"
		"ExitCode:0">>]),
    ?line ok = file:rename(MainScript, MainScript ++ "_without_header"),

    %%%%%%%
    %% Create and run scripts with emulator flags
    AltBase = "archive_script_alternate_main",
    AltScript = filename:join([PrivDir, AltBase]),
    ?line ok = file:write_file(AltScript,
			       [Shebang, "\n",
				Mode, "\n",
				Flags, " -escript main archive_script_main2\n",
				ArchiveBin]),
    ?line ok = file:write_file_info(AltScript, OrigFI),

    ?line run(PrivDir, AltBase ++  " -arg1 arg2 arg3",
	      [<<"main2:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
		"dummy:[{archive_script_dummy,[\"bar\"]}]\n"
		"priv:{ok,<<\"Some private data...\\n\">>}\n"
		"ExitCode:0">>]),

    ok.

%% Test the correction of OTP-10071
%% The errors identified are
%%
%% * If primary archive was named "xxx", then a file in the same
%%   directory named "xxxyyy" would be interpreted as a file named yyy
%%   inside the archive.
%%
%% * erl_prim_loader did not correctly create and normalize absolute
%%   paths for primary archive and files inside it, so unless given
%%   with exact same path files inside the archive would not be
%%   found. E.g. if escript was started as ./xxx then "xxx/file" would
%%   not be found since erl_prim_loader would try to match
%%   /full/path/to/xxx with /full/path/to/./xxx. Same problem with
%%   ../
%%
%% * Depending on how the primary archive was built,
%%   erl_prim_loader:list_dir/1 would sometimes return an empty string
%%   inside the file list. This was a virtual element representing the
%%   top directory of the archive. This shall not occur.
%%
archive_script_file_access(Config) when is_list(Config) ->
    %% Copy the orig files to priv_dir
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    MainMod = "archive_script_file_access",
    MainSrc = MainMod ++ ".erl",
    MainBeam = MainMod ++ ".beam",

    Archive = filename:join([PrivDir, "archive_script_file_access.zip"]),
    ?line {ok, _} = zip:create(Archive, ["archive_script_file_access"],
			       [{compress, []}, {cwd, DataDir}]),
    ?line {ok, _} = zip:extract(Archive, [{cwd, PrivDir}]),
    TopDir = filename:join([PrivDir, "archive_script_file_access"]),

    %% Compile the code
    ?line ok = compile_files([MainSrc], TopDir, TopDir),

    %% First, create a file structure which will be included in the archive:
    %%
    %% dir1/
    %% dir1/subdir1/
    %% dir1/subdir1/file1
    %%
    {ok, OldDir} = file:get_cwd(),
    ok = file:set_cwd(TopDir),
    DummyDir = "dir1",
    DummySubDir = filename:join(DummyDir, "subdir1"),
    RelDummyFile = filename:join(DummySubDir, "file1"),
    DummyFile = filename:join(TopDir,RelDummyFile),
    ok = filelib:ensure_dir(DummyFile),
    ok = file:write_file(DummyFile, ["foo\nbar\nbaz"]),

    %% 1. Create zip archive by adding the dummy file and the beam
    %%    file as binaries to zip.
    %%
    %% This used to provoke the following issues when the script was run as
    %% "./<script_name>":
    %% a. erl_prim_loader:read_file_info/1 returning 'error'
    %% b. erl_prim_loader:list_dir/1 returning {ok, ["dir1", [], "file1"]}
    %%    leading to an infinite loop in reltool_target:spec_dir/1
    Files1 =
	lists:map(fun(Filename) ->
			  {ok, Bin} = file:read_file(Filename),
			  {Filename,Bin}
		  end,
		  [RelDummyFile,MainBeam]),
    {ok, {"mem", Bin1}} = zip:create("mem", Files1, [memory]),

    %% Create the escript
    ScriptName1 = "archive_script_file_access1",
    Script1 = filename:join([PrivDir, ScriptName1]),
    Flags = "-escript main " ++ MainMod,
    ok = escript:create(Script1,[shebang,{emu_args,Flags},{archive,Bin1}]),
    ok = file:change_mode(Script1,8#00744),

    %% Also add a dummy file in the same directory with the same name
    %% as the script except is also has an extension. This used to
    %% cause erl_prim_loader to believe it was a file inside the
    %% script.
    ok = file:write_file(Script1 ++ ".extension",
			 <<"same name as script, but with extension">>),

    %% Change to script's directory and run it as "./<script_name>"
    ok = file:set_cwd(PrivDir),
    do_run(PrivDir, "./" ++ ScriptName1,
	   [<<"file_access:[]\n",
	      "ExitCode:0">>]),
    ok = file:set_cwd(TopDir),


    %% 2. Create zip archive by letting zip read the files from the file system
    %%
    %% The difference compared to the archive_script_file_access1 is
    %% that this will have a file element for each directory in the
    %% archive - while archive_script_file_access1 will only have a
    %% file element per regular file.
    Files2 = [DummyDir,MainBeam],
    {ok, {"mem", Bin2}} = zip:create("mem", Files2, [memory]),

    %% Create the escript
    ScriptName2 = "archive_script_file_access2",
    Script2 = filename:join([PrivDir, ScriptName2]),
    ok = escript:create(Script2,[shebang,{emu_args,Flags},{archive,Bin2}]),
    ok = file:change_mode(Script2,8#00744),

    %% Also add a dummy file in the same directory with the same name
    %% as the script except is also has an extension. This used to
    %% cause erl_prim_loader to believe it was a file inside the
    %% script.
    ok = file:write_file(Script2 ++ ".extension",
			 <<"same name as script, but with extension">>),

    %% Change to script's directory and run it as "./<script_name>"
    ok = file:set_cwd(PrivDir),
    do_run(PrivDir, "./" ++ ScriptName2,
	   [<<"file_access:[]\n",
	      "ExitCode:0">>]),
    ok = file:set_cwd(OldDir).

compile_app(TopDir, AppName) ->
    AppDir = filename:join([TopDir, AppName]),
    SrcDir = filename:join([AppDir, "src"]),
    OutDir = filename:join([AppDir, "ebin"]),
    ?line {ok, Files} = file:list_dir(SrcDir),
    compile_files(Files, SrcDir, OutDir).

compile_files([File | Files], SrcDir, OutDir) ->
    case filename:extension(File) of
	".erl" ->
	    AbsFile = filename:join([SrcDir, File]),
	    case compile:file(AbsFile, [{outdir, OutDir}]) of
		{ok, _Mod} ->
		    compile_files(Files, SrcDir, OutDir);
		Error ->
		    {compilation_error, AbsFile, OutDir, Error}
	    end;
	_ ->
	    compile_files(Files, SrcDir, OutDir)
    end;
compile_files([], _, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

epp(Config) when is_list(Config) ->
    Data = ?config(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    ?line run(Dir, "factorial_epp 5",
	      <<"factorial 5 = 120\nExitCode:0">>),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_and_extract(Config) when is_list(Config) ->
    {NewFile, FileInfo,
     EmuArg, Source,
     _ErlBase, ErlCode,
     _BeamBase, BeamCode,
     ArchiveBin} =
	prepare_creation("create_and_extract", Config),

    Bodies =
	[[{source, ErlCode}],
	 [{beam, BeamCode}],
	 [{archive, ArchiveBin}]],

    %% Verify all combinations of scripts with shebangs
    [verify_sections(NewFile, FileInfo, S ++ C ++ E ++ B) ||
	S <- [[{shebang, default}],
	      [{shebang, "/usr/bin/env     escript"}]],
	C <- [[],
	      [{comment, undefined}],
	      [{comment, default}],
	      [{comment, "This is a nonsense comment"}]],
	E <- [[],
	      [{emu_args, undefined}],
	      [{emu_args, EmuArg}]],
	B <- [[{source, Source}] | Bodies]],

    %% Verify all combinations of scripts without shebangs
    [verify_sections(NewFile, FileInfo, S ++ C ++ E ++ B) ||
	S <- [[], [{shebang, undefined}]],
	C <- [[], [{comment, undefined}]],
	E <- [[], [{emu_args, undefined}]],
	B <- Bodies],

    %% Verify the compile_source option
    file:delete(NewFile),
    ?line ok = escript:create(NewFile, [{source, Source}]),
    ?line {ok, [_, _, _, {source, Source}]} = escript:extract(NewFile, []),
    ?line {ok, [_, _, _, {source, BeamCode2}]} =
	escript:extract(NewFile, [compile_source]),
    verify_sections(NewFile, FileInfo,
		    [{shebang, default},
		     {comment, default},
		     {beam, BeamCode2}]),

    file:delete(NewFile),
    ok.

prepare_creation(Base, Config) ->
    %% Read the source
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    OrigFile = filename:join([DataDir,"emulator_flags"]),
    ?line {ok, FileInfo} = file:read_file_info(OrigFile),
    NewFile = filename:join([PrivDir, Base]),
    ?line {ok, [{shebang, default},
		{comment, _},
		{emu_args, EmuArg},
		{source, Source}]} =
	escript:extract(OrigFile, []),

    %% Compile the code
    ErlFile = NewFile ++ ".erl",
    ErlCode = list_to_binary(["\n-module(", Base, ").\n",
			      "-export([main/1]).\n\n",
			      Source, "\n\n"]),
    ?line ok = file:write_file(ErlFile, ErlCode),

    %% Compile the code
    ?line {ok, _Mod, BeamCode} =
	compile:file(ErlFile, [binary, debug_info]),

    %% Create an archive
    ?line {ok, {_, ArchiveBin}} =
	zip:create("dummy_archive_name",
		   [{Base ++ ".erl", ErlCode},
		    {Base ++ ".beam", BeamCode}],
		   [{compress, []}, memory]),
    {NewFile, FileInfo,
     EmuArg, Source,
     Base ++ ".erl", ErlCode,
     Base ++ ".beam", BeamCode,
     ArchiveBin}.

verify_sections(File, FileInfo, Sections) ->
    io:format("~p:verify_sections(\n\t~p,\n\t~p,\n\t~p).\n",
	      [?MODULE, File, FileInfo, Sections]),

    %% Create
    file:delete(File),
    ?line ok = escript:create(File, Sections),
    ?line ok = file:write_file_info(File, FileInfo),

    %% Run
    Dir = filename:absname(filename:dirname(File)),
    Base = filename:basename(File),

    HasArg = fun(Tag) ->
		     case lists:keysearch(Tag, 1, Sections) of
			 false -> false;
			 {value, {_, undefined}} -> false;
			 {value, _} -> true
		     end
	     end,
    ExpectedMain = <<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n">>,
    ExpectedOutput =
	case HasArg(emu_args) of
	    true ->
		<<"nostick:[{nostick,[]}]\n"
		  "mnesia:[{mnesia,[\"dir\",\"a/directory\"]},{mnesia,[\"debug\",\"verbose\"]}]\n"
		  "ERL_FLAGS=false\n"
		  "unknown:[]\n"
		  "ExitCode:0">>;
	    false ->
		<<"nostick:[]\nmnesia:[]\nERL_FLAGS=false\nunknown:[]\nExitCode:0">>
	end,

    InputArgs = Base ++ " -arg1 arg2 arg3",
    Expected = <<ExpectedMain/binary, ExpectedOutput/binary>>,
    case HasArg(shebang) of
	true ->
	    ?line run(Dir, InputArgs, [Expected]);
	false ->
	    ?line run_with_opts(Dir, [], InputArgs, [Expected])
    end,

    %% Verify
    ?line {ok, Bin} = escript:create(binary, Sections),
    ?line {ok, Read} = file:read_file(File),
    ?line Bin = Read, % Assert

    Normalized = normalize_sections(Sections),
    ?line {ok, Extracted} = escript:extract(File, []),
    io:format("Normalized; ~p\n", [Normalized]),
    io:format("Extracted ; ~p\n", [Extracted]),
    ?line Normalized = Extracted, % Assert
    ok.

normalize_sections(Sections) ->
    AtomToTuple =
	fun(Val) ->
		if
		    is_atom(Val) -> {Val, default};
		    true -> Val
		end
	end,
    case lists:map(AtomToTuple, [{K, V} || {K, V} <- Sections, V =/= undefined]) of
	    [{shebang, Shebang} | Rest] ->
		[{shebang, Shebang} |
		 case Rest of
		     [{comment, Comment} | Rest2] ->
			 [{comment, Comment} |
			  case Rest2 of
			      [{emu_args, EmuArgs}, Body] ->
				  [{emu_args, EmuArgs}, Body];
			      [Body] ->
				  [{emu_args, undefined}, Body]
			  end
			 ];
		     [{emu_args, EmuArgs}, Body] ->
			 [{comment, undefined}, {emu_args, EmuArgs}, Body];
		     [Body] ->
			 [{comment, undefined}, {emu_args, undefined}, Body]
		 end
		];
	    [Body] ->
		[{shebang, undefined}, {comment, undefined}, {emu_args, undefined}, Body]
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foldl(Config) when is_list(Config) ->
    {NewFile, _FileInfo,
     _EmuArg, _Source,
     ErlBase, ErlCode,
     BeamBase, _BeamCode,
     ArchiveBin} =
	prepare_creation("foldl", Config),

    Collect = fun(Name, GetInfo, GetBin, Acc) ->
		      [{Name, GetInfo(), GetBin()} | Acc]
	      end,

    %% Get line numbers and the file attribute right
    SourceFile = NewFile ++ ".erl",
    <<_:1/binary, ErlCode2/binary>> = ErlCode,
    ?line ok = file:write_file(SourceFile, ErlCode2),
    ?line {ok, _Mod, BeamCode} =
	compile:file(SourceFile, [binary, debug_info]),

    %% Verify source script
    ?line ok = escript:create(SourceFile, [{source, ErlCode}]),
    ?line {ok, [{".", _, BeamCode2}]}
	= escript_foldl(Collect, [], SourceFile),

    ?line {ok, Abstr} = beam_lib:chunks(BeamCode, [abstract_code]),
    ?line {ok, Abstr2} = beam_lib:chunks(BeamCode2, [abstract_code]),
    %% io:format("abstr1=~p\n", [Abstr]),
    %% io:format("abstr2=~p\n", [Abstr2]),
    ?line Abstr = Abstr2, % Assert

    %% Verify beam script
    ?line ok = escript:create(NewFile, [{beam, BeamCode}]),
    ?line {ok, [{".", _, BeamCode}]}
	= escript_foldl(Collect, [], NewFile),

    %% Verify archive scripts
    ?line ok = escript:create(NewFile, [{archive, ArchiveBin}]),
    ?line {ok, [{BeamBase, #file_info{}, _},
		{ErlBase, #file_info{}, _}]}
	= escript_foldl(Collect, [], NewFile),

    ArchiveFiles = [{ErlBase, ErlCode}, {BeamBase, BeamCode}],
    ?line ok = escript:create(NewFile, [{archive, ArchiveFiles, []}]),
    ?line {ok, [{BeamBase, _, _},
	      {ErlBase, _, _}]}
	= escript_foldl(Collect, [], NewFile),

    ok.

escript_foldl(Fun, Acc, File) ->
    code:ensure_loaded(zip),
    case erlang:function_exported(zip, foldl, 3) of
	true ->
	    emulate_escript_foldl(Fun, Acc, File);
	false ->
	    escript:foldl(Fun, Acc, File)
    end.

emulate_escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, [compile_source]) of
	{ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
	    case Body of
		{source, BeamCode} ->
		    GetInfo = fun() -> file:read_file_info(File) end,
		    GetBin = fun() -> BeamCode end,
		    {ok, Fun(".", GetInfo, GetBin, Acc)};
		{beam, BeamCode} ->
		    GetInfo = fun() -> file:read_file_info(File) end,
		    GetBin = fun() -> BeamCode end,
		    {ok, Fun(".", GetInfo, GetBin, Acc)};
		{archive, ArchiveBin} ->
		    zip:foldl(Fun, Acc, {File, ArchiveBin})
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overflow(Config) when is_list(Config) ->
    Data = ?config(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    ?line run(Dir, "arg_overflow",
	      [<<"ExitCode:0">>]),
    ?line run(Dir, "linebuf_overflow",
	      [<<"ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Dir, Cmd0, Expected0) ->
    Expected = iolist_to_binary(expected_output(Expected0, Dir)),
    Cmd = case os:type() of
	      {win32,_} -> "escript " ++ filename:nativename(Dir) ++ "\\" ++ Cmd0;
	      _ -> Cmd0
	  end,
    do_run(Dir, Cmd, Expected).

run_with_opts(Dir, Opts, Cmd0, Expected) ->
    Cmd = case os:type() of
	      {win32,_} -> "escript " ++ Opts ++ " " ++ filename:nativename(Dir) ++ "\\" ++ Cmd0;
	      _ -> "escript " ++ Opts ++ " " ++ Dir ++ "/" ++ Cmd0
	  end,
    do_run(Dir, Cmd, Expected).

do_run(Dir, Cmd, Expected0) ->
    io:format("Run: ~p\n", [Cmd]),
    Expected = iolist_to_binary(expected_output(Expected0, Dir)),

    Env = [{"PATH",Dir++":"++os:getenv("PATH")}],
    Port = open_port({spawn,Cmd}, [exit_status,eof,in,{env,Env}]),
    Res = get_data(Port, []),
    receive
	{Port,{exit_status,ExitCode}} ->
	    case iolist_to_binary([Res,"ExitCode:"++integer_to_list(ExitCode)]) of
		Expected ->
		    ok;
		Actual ->
		    io:format("Expected: ~p\n", [Expected]),
		    io:format("Actual:   ~p\n", [Actual]),
		    ?t:fail()
	    end
    end.

get_data(Port, SoFar) ->
    receive
	{Port,{data,Bytes}} ->
	    get_data(Port, [SoFar|Bytes]);
	{Port,eof} ->
	    erlang:port_close(Port),
	    SoFar
    end.

expected_output([data_dir|T], Data) ->
    Slash = case os:type() of
		{win32,_} -> "\\";
		_ -> "/"
	    end,
    [filename:nativename(Data)++Slash|expected_output(T, Data)];
expected_output([H|T], Data) ->
    [H|expected_output(T, Data)];
expected_output([], _) ->
    [];
expected_output(Bin, _) when is_binary(Bin) ->
    Bin.

