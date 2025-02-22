%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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
	 emulator_flags_no_shebang/1,
	 two_lines/1,
	 module_script/1,
	 beam_script/1,
	 legacy_archive_script/1,
	 archive_script/1,
	 epp/1,
	 create_and_extract/1,
	 overflow/1,
	 verify_sections/4,
         unicode/1,
         bad_io_server/1,
         bypass_unicode_conversion/1
	]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [basic, errors, strange_name, emulator_flags,
     emulator_flags_no_shebang, two_lines,
     module_script, beam_script,
     legacy_archive_script, archive_script,
     epp, create_and_extract, overflow,
     unicode, bad_io_server,
     bypass_unicode_conversion].

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
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "factorial 5",
	<<"factorial 5 = 120\nExitCode:0">>),
    run(Config, Dir, "factorial_compile 10",
	<<"factorial 10 = 3628800\nExitCode:0">>),
    run(Config, Dir, "factorial_compile_main 7",
	<<"factorial 7 = 5040\nExitCode:0">>),
    run(Config, Dir, "factorial_warning 20",
        [data_dir,<<"factorial_warning:12:1: Warning: function bar/0 is unused\n"
                    "%   12| bar() ->\n"
                    "%     | ^\n\n"
                    "factorial 20 = 2432902008176640000\n"
                    "ExitCode:0">>]),
    run_with_opts(Config, Dir, "-i", "factorial_warning 20",
                  [data_dir,<<"factorial_warning:12:1: Warning: function bar/0 is unused\n"
                              "factorial 20 = 2432902008176640000\nExitCode:0">>]),
    Warnings = [data_dir,<<"factorial_warning:12:1: Warning: function bar/0 is unused\n"
                           "%   12| bar() ->\n"
                           "%     | ^\n\n"
                           "ExitCode:0">>],
    run_with_opts(Config, Dir, "-s", "factorial_warning", Warnings),
    run_with_opts(Config, Dir, "-s -i", "factorial_warning", Warnings),
    run_with_opts(Config, Dir, "-c -s", "factorial_warning", Warnings),
    run(Config, Dir, "filesize "++filename:join(proplists:get_value(data_dir, Config),"filesize"),
	[data_dir,<<"filesize:11:1: Warning: function id/1 is unused\n"
                    "%   11| id(I) -> I.\n"
                    "%     | ^\n"
                    "\n"
                    "324\n"
                    "ExitCode:0">>]),
    run(Config, Dir, "test_script_name",
	[data_dir,<<"test_script_name\nExitCode:0">>]),
    run(Config, Dir, "tail_rec 1000",
	[<<"ok\nExitCode:0">>]),

    %% We expect the trap_exit flag for the process to be false,
    %% since that is the default state for newly spawned processes.
    run(Config, Dir, "trap_exit",
	<<"false\nExitCode:0">>),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

errors(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "compile_error",
	[data_dir,<<"compile_error:5:12: syntax error before: '*'\n">>,
	 data_dir,<<"compile_error:8:9: syntax error before: blarf\n">>,
	 <<"escript: There were compilation errors.\nExitCode:127">>]),
    CompileErrors = [data_dir,<<"lint_error:6:1: function main/1 already defined\n"
                                "%    6| main(Args) ->\n"
                                "%     | ^\n\n">>,
                     data_dir,("lint_error:8:10: variable 'ExitCode' is unbound\n"
                               "%    8|     halt(ExitCode).\n"
                               "%     |          ^\n\n"),
                     <<"escript: There were compilation errors.\nExitCode:127">>],
    run(Config, Dir, "lint_error", CompileErrors),
    run_with_opts(Config, Dir, "-i", "lint_error",
                  [data_dir,<<"lint_error:6:1: function main/1 already defined\n">>,
                   data_dir,"lint_error:8:10: variable 'ExitCode' is unbound\n",
                   <<"escript: There were compilation errors.\nExitCode:127">>]),
    run_with_opts(Config, Dir, "-s", "lint_error", CompileErrors),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strange_name(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "strange.name -arg1 arg2 arg3",
	[<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
	   "ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emulator_flags(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "emulator_flags -arg1 arg2 arg3",
	[<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
	   "nostick:[{nostick,[]}]\n"
	   "mnesia:[{mnesia,[\"dir\",\"a/directory\"]},{mnesia,[\"debug\",\"verbose\"]}]\n"
	   "ERL_FLAGS=false\n"
	   "unknown:[]\n"
	   "ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

two_lines(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "two_lines -arg1 arg2 arg3",
	[<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
	   "ERL_FLAGS=false\n"
	   "unknown:[]\n"
	   "ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emulator_flags_no_shebang(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    %% Need run_with_opts, to always use "escript" explicitly
    run_with_opts(Config, Dir, "", "emulator_flags_no_shebang -arg1 arg2 arg3",
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
    Data = proplists:get_value(data_dir, Config),
    OrigFile = filename:join([Data,"emulator_flags"]),
    {ok, OrigBin} = file:read_file(OrigFile),
    [Shebang, Mode, Flags | Source] = string:tokens(binary_to_list(OrigBin), "\n"),
    {ok, OrigFI} = file:read_file_info(OrigFile),

    %% Write source file
    Priv = proplists:get_value(priv_dir, Config),
    Dir = filename:absname(Priv), % Get rid of trailing slash.
    Base = "module_script",
    ErlFile = filename:join([Priv, Base ++ ".erl"]),
    ErlCode = ["\n-module(", Base, ").\n",
	       "-export([main/1]).\n\n",
	       string:join(Source, "\n"),
	       "\n"],
    ok = file:write_file(ErlFile, ErlCode),

%%%%%%%
    %% Create and run scripts without emulator flags

    %% With shebang
    NoArgsBase = Base ++ "_no_args_with_shebang",
    NoArgsFile = filename:join([Priv, NoArgsBase]),
    ok = file:write_file(NoArgsFile,
			 [Shebang, "\n",
			  ErlCode]),
    ok = file:write_file_info(NoArgsFile, OrigFI),

    run(Config, Dir, NoArgsBase ++ " -arg1 arg2 arg3",
	[<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
	   "nostick:[]\n"
	   "mnesia:[]\n"
	   "ERL_FLAGS=false\n"
	   "unknown:[]\n"
	   "ExitCode:0">>]),

    run_with_opts(Config, Dir, "", NoArgsBase ++  " -arg1 arg2 arg3",
		  [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		     "nostick:[]\n"
		     "mnesia:[]\n"
		     "ERL_FLAGS=false\n"
		     "unknown:[]\n"
		     "ExitCode:0">>]),

    %% Without shebang
    NoArgsBase2 = Base ++ "_no_args_without_shebang",
    NoArgsFile2 = filename:join([Priv, NoArgsBase2]),
    ok = file:write_file(NoArgsFile2,
			 ["Something else than shebang!!!", "\n",
			  ErlCode]),
    ok = file:write_file_info(NoArgsFile2, OrigFI),

    run_with_opts(Config, Dir, "", NoArgsBase2 ++  " -arg1 arg2 arg3",
		  [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		     "nostick:[]\n"
		     "mnesia:[]\n"
		     "ERL_FLAGS=false\n"
		     "unknown:[]\n"
		     "ExitCode:0">>]),

    %% Plain module without header
    NoArgsBase3 = Base ++ "_no_args_without_header",
    NoArgsFile3 = filename:join([Priv, NoArgsBase3]),
    ok = file:write_file(NoArgsFile3, [ErlCode]),
    ok = file:write_file_info(NoArgsFile3, OrigFI),

    run_with_opts(Config, Dir, "", NoArgsBase3 ++  " -arg1 arg2 arg3",
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
    ok = file:write_file(ArgsFile,
			 [Shebang, "\n",
			  Mode, "\n",
			  Flags, "\n",
			  ErlCode]),
    ok = file:write_file_info(ArgsFile, OrigFI),

    run(Config, Dir, ArgsBase ++  " -arg1 arg2 arg3",
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
    Data = proplists:get_value(data_dir, Config),
    OrigFile = filename:join([Data,"emulator_flags"]),
    {ok, OrigBin} = file:read_file(OrigFile),
    [Shebang, Mode, Flags | Source] = string:tokens(binary_to_list(OrigBin), "\n"),
    {ok, OrigFI} = file:read_file_info(OrigFile),

    %% Write source file
    Priv = proplists:get_value(priv_dir, Config),
    Dir = filename:absname(Priv), % Get rid of trailing slash.
    Base = "beam_script",
    ErlFile = filename:join([Priv, Base ++ ".erl"]),
    ok = file:write_file(ErlFile,
			 ["\n-module(", Base, ").\n",
			  "-export([main/1]).\n\n",
			  string:join(Source, "\n"),
			  "\n"]),

    %% Compile the code
    {ok, _Mod, BeamCode} = compile:file(ErlFile, [binary]),

%%%%%%%
    %% Create and run scripts without emulator flags

    %% With shebang
    NoArgsBase = Base ++ "_no_args_with_shebang",
    NoArgsFile = filename:join([Priv, NoArgsBase]),
    ok = file:write_file(NoArgsFile,
			 [Shebang, "\n",
			  BeamCode]),
    ok = file:write_file_info(NoArgsFile, OrigFI),

    run(Config, Dir, NoArgsBase ++  " -arg1 arg2 arg3",
	[<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
	   "nostick:[]\n"
	   "mnesia:[]\n"
	   "ERL_FLAGS=false\n"
	   "unknown:[]\n"
	   "ExitCode:0">>]),

    run_with_opts(Config, Dir, "", NoArgsBase ++  " -arg1 arg2 arg3",
		  [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		     "nostick:[]\n"
		     "mnesia:[]\n"
		     "ERL_FLAGS=false\n"
		     "unknown:[]\n"
		     "ExitCode:0">>]),

    %% Without shebang
    NoArgsBase2 = Base ++ "_no_args_without_shebang",
    NoArgsFile2 = filename:join([Priv, NoArgsBase2]),
    ok = file:write_file(NoArgsFile2,
			 ["Something else than shebang!!!", "\n",
			  BeamCode]),
    ok = file:write_file_info(NoArgsFile2, OrigFI),

    run_with_opts(Config, Dir, "", NoArgsBase2 ++  " -arg1 arg2 arg3",
		  [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		     "nostick:[]\n"
		     "mnesia:[]\n"
		     "ERL_FLAGS=false\n"
		     "unknown:[]\n"
		     "ExitCode:0">>]),

    %% Plain beam file without header
    NoArgsBase3 = Base ++ "_no_args_without_header",
    NoArgsFile3 = filename:join([Priv, NoArgsBase3]),
    ok = file:write_file(NoArgsFile3, [BeamCode]),
    ok = file:write_file_info(NoArgsFile3, OrigFI),

    run_with_opts(Config, Dir, "", NoArgsBase3 ++  " -arg1 arg2 arg3",
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
    ok = file:write_file(ArgsFile,
			 [Shebang, "\n",
			  Mode, "\n",
			  Flags, "\n",
			  BeamCode]),
    ok = file:write_file_info(ArgsFile, OrigFI),

    run(Config, Dir, ArgsBase ++  " -arg1 arg2 arg3",
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
%% Archives in this test have the pre-OTP-28 format.
legacy_archive_script(Config) when is_list(Config) ->
    %% Copy the orig files to priv_dir
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Archive = filename:join([PrivDir, "archive_script.zip"]),
    {ok, _} = zip:create(Archive, ["archive_script"],
			 [{compress, []}, {cwd, DataDir}]),
    {ok, _} = zip:extract(Archive, [{cwd, PrivDir}]),
    TopDir = filename:join([PrivDir, "archive_script"]),

    %% Compile the code
    ok = compile_app(TopDir, "archive_script_dict"),
    ok = compile_app(TopDir, "archive_script_dummy"),
    {ok, MainFiles} = file:list_dir(TopDir),
    ok = compile_files(MainFiles, TopDir, TopDir),

    %% Create the archive
    {ok, TopFiles} = file:list_dir(TopDir),
    {ok, {_, ArchiveBin}} = zip:create(Archive, TopFiles,
				       [memory, {compress, []}, {cwd, TopDir}]),

    %% Read the source script
    OrigFile = filename:join([DataDir, "emulator_flags"]),
    {ok, OrigBin} = file:read_file(OrigFile),
    [Shebang, Mode, _Flags | _Source] =
	string:tokens(binary_to_list(OrigBin), "\n"),
    Flags = "%%! -archive_script_dict foo bar"
	" -archive_script_dict foo"
	" -archive_script_dummy bar",
    {ok, OrigFI} = file:read_file_info(OrigFile),

%%%%%%%
    %% Create and run scripts without emulator flags
    MainBase = "archive_script_main",
    MainScript = filename:join([PrivDir, MainBase]),

    %% With shebang
    ok = file:write_file(MainScript,
			 [Shebang, "\n",
			  Flags, "\n",
			  ArchiveBin]),
    ok = file:write_file_info(MainScript, OrigFI),

    run(Config, PrivDir, MainBase ++  " -legacy_arg1 arg2 arg3",
	[<<"main:[\"-legacy_arg1\",\"arg2\",\"arg3\"]\n"
	   "dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
	   "dummy:[{archive_script_dummy,[\"bar\"]}]\n"
	   "extract: ok\n"
	   "ExitCode:0">>]),

    run_with_opts(Config, PrivDir, "", MainBase ++  " -legacy_arg1 arg2 arg3",
		  [<<"main:[\"-legacy_arg1\",\"arg2\",\"arg3\"]\n"
		     "dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
		     "dummy:[{archive_script_dummy,[\"bar\"]}]\n"
		     "extract: ok\n"
		     "ExitCode:0">>]),

    ok = file:rename(MainScript, MainScript ++ "_with_shebang"),

    %% Without shebang (no flags)
    ok = file:write_file(MainScript,
			 ["%% Something else than shebang!!!", "\n",
			  ArchiveBin]),
    ok = file:write_file_info(MainScript, OrigFI),

    run_with_opts(Config, PrivDir, "", MainBase ++  " -legacy_arg1 arg2 arg3",
		  [<<"main:[\"-legacy_arg1\",\"arg2\",\"arg3\"]\n"
		     "dict:[]\n"
		     "dummy:[]\n"
		     "extract: ok\n"
		     "ExitCode:0">>]),
    ok = file:rename(MainScript, MainScript ++ "_without_shebang"),

    %% Plain archive without header (no flags)

    ok = file:write_file(MainScript, [ArchiveBin]),
    ok = file:write_file_info(MainScript, OrigFI),

    run_with_opts(Config, PrivDir, "", MainBase ++  " -legacy_arg1 arg2 arg3",
		  [<<"main:[\"-legacy_arg1\",\"arg2\",\"arg3\"]\n"
		     "dict:[]\n"
		     "dummy:[]\n"
		     "extract: ok\n"
		     "ExitCode:0">>]),
    ok = file:rename(MainScript, MainScript ++ "_without_header"),

%%%%%%%
    %% Create and run scripts with emulator flags
    AltBase = "archive_script_alternate_main",
    AltScript = filename:join([PrivDir, AltBase]),
    ok = file:write_file(AltScript,
			 [Shebang, "\n",
			  Mode, "\n",
			  Flags, " -escript main archive_script_main2\n",
			  ArchiveBin]),
    ok = file:write_file_info(AltScript, OrigFI),

    run(Config, PrivDir, AltBase ++  " -legacy_arg1 arg2 arg3",
	[<<"main2:[\"-legacy_arg1\",\"arg2\",\"arg3\"]\n"
	   "dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
	   "dummy:[{archive_script_dummy,[\"bar\"]}]\n"
	   "extract: ok\n"
	   "ExitCode:0">>]),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create an archive file containing two entire applications plus two
%% alternate main modules. Generate a new escript containing the archive
%% (with .app and .beam files and) and the escript header.
%% Archives in this test have the new OTP-28 format.
archive_script(Config) when is_list(Config) ->
    %% Copy the orig files to priv_dir.
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Archive = filename:join([PrivDir, "archive_script.zip"]),
    {ok, _} = zip:create(Archive, ["archive_script"],
			 [{compress, []}, {cwd, DataDir}]),
    {ok, _} = zip:extract(Archive, [{cwd, PrivDir}]),
    TopDir = filename:join([PrivDir, "archive_script"]),

    %% Compile the code.
    ok = compile_app(TopDir, "archive_script_dict"),
    ok = compile_app(TopDir, "archive_script_dummy"),
    {ok, MainFiles} = file:list_dir(TopDir),
    ok = compile_files(MainFiles, TopDir, TopDir),

    %% Read the source script.
    OrigFile = filename:join([DataDir, "emulator_flags"]),
    Flags = "-archive_script_dict foo bar"
	" -archive_script_dict foo"
	" -archive_script_dummy bar",
    {ok, OrigFI} = file:read_file_info(OrigFile),

%%%%%%%
    %% Create and run scripts without emulator flags.
    MainBase = "archive_script_main",
    MainScript = filename:join([PrivDir, MainBase]),

    %% With shebang.
    All = filelib:wildcard(filename:join(PrivDir, "archive_script/**")),
    Beams = filelib:wildcard(filename:join(PrivDir, "**/*.beam")),
    Files = [F || F <- All, filelib:is_regular(F)] -- Beams,

    ok = escript:create(MainScript, [shebang,
                                     {emu_args, Flags},
                                     {modules, Beams},
                                     {files, Files}]),
    ok = file:write_file_info(MainScript, OrigFI),

    run(Config, PrivDir, MainBase ++  " -arg1 arg2 arg3",
        [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
           "dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
           "dummy:[{archive_script_dummy,[\"bar\"]}]\n"
           "extract: ok\n"
           "ExitCode:0">>]),

    run_with_opts(Config, PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
                  [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
                     "dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
                     "dummy:[{archive_script_dummy,[\"bar\"]}]\n"
                     "extract: ok\n"
                     "ExitCode:0">>]),

    ok = file:rename(MainScript, MainScript ++ "_with_shebang"),

    %% Without shebang (no flags).
    ok = escript:create(MainScript, [{comment, "Something else than shebang!!!"},
                                     {modules, Beams},
                                     {files, Files}]),
    ok = file:write_file_info(MainScript, OrigFI),

    run_with_opts(Config, PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
		  [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		     "dict:[]\n"
		     "dummy:[]\n"
		     "extract: ok\n"
		     "ExitCode:0">>]),
    ok = file:rename(MainScript, MainScript ++ "_without_shebang"),

    %% Plain archive without header (no flags).

    ok = escript:create(MainScript, [{modules, Beams}, {files, Files}]),
    ok = file:write_file_info(MainScript, OrigFI),

    run_with_opts(Config, PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
		  [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		     "dict:[]\n"
		     "dummy:[]\n"
		     "extract: ok\n"
		     "ExitCode:0">>]),
    ok = file:rename(MainScript, MainScript ++ "_without_header"),

%%%%%%%
    %% Create and run scripts with emulator flags.
    AltBase = "archive_script_alternate_main",
    AltScript = filename:join([PrivDir, AltBase]),
    ok = escript:create(AltScript,
                        [shebang,
                         {comment, "-*- erlang -*-"},
                         {emu_args, Flags ++ " -escript main archive_script_main2"},
                         {modules, Beams},
                         {files, Files}]),
    ok = file:write_file_info(AltScript, OrigFI),

    run(Config, PrivDir, AltBase ++  " -arg1 arg2 arg3",
	[<<"main2:[\"-arg1\",\"arg2\",\"arg3\"]\n"
	   "dict:[{archive_script_dict,[\"foo\",\"bar\"]},{archive_script_dict,[\"foo\"]}]\n"
	   "dummy:[{archive_script_dummy,[\"bar\"]}]\n"
	   "extract: ok\n"
	   "ExitCode:0">>]),

    ok.

compile_app(TopDir, AppName) ->
    AppDir = filename:join([TopDir, AppName]),
    SrcDir = filename:join([AppDir, "src"]),
    OutDir = filename:join([AppDir, "ebin"]),
    {ok, Files} = file:list_dir(SrcDir),
    compile_files(Files, SrcDir, OutDir).

compile_files([File | Files], SrcDir, OutDir) ->
    case filename:extension(File) of
	".erl" ->
	    AbsFile = filename:join([SrcDir, File]),
	    case compile:file(AbsFile, [{outdir, OutDir},report_errors]) of
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
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "factorial_epp 5",
	<<"factorial 5 = 120\nExitCode:0">>),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Archives in this test have the new (OTP 28) format.
create_and_extract(Config) when is_list(Config) ->
    {NewFile, FileInfo,
     EmuArg, Source,
     ErlFile, ErlCode,
     _BeamFile, BeamCode} =
	prepare_creation("create_and_extract", Config),

    Bodies =
	[[{source, ErlCode}],
	 [{beam, BeamCode}],
	 [{modules, [BeamCode]},
          {files, [{filename:basename(ErlFile), ErlCode}]}]],

    %% Verify all combinations of scripts with shebangs
    [verify_sections(Config, NewFile, FileInfo, S ++ C ++ E ++ B) ||
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
    [verify_sections(Config, NewFile, FileInfo, S ++ C ++ E ++ B) ||
	S <- [[], [{shebang, undefined}]],
	C <- [[], [{comment, undefined}]],
	E <- [[], [{emu_args, undefined}]],
	B <- Bodies],

    %% Verify the compile_source option
    file:delete(NewFile),
    ok = escript:create(NewFile, [{source, Source}]),
    {ok, [_, _, _, {source, Source}]} = escript:extract(NewFile, []),
    {ok, [_, _, _, {source, BeamCode2}]} =
	escript:extract(NewFile, [compile_source]),
    verify_sections(Config, NewFile, FileInfo,
		    [{shebang, default},
		     {comment, default},
		     {beam, BeamCode2}]),

    file:delete(NewFile),
    ok.

prepare_creation(Base, Config) ->
    %% Read the source
    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config),
    OrigFile = filename:join([DataDir,"emulator_flags"]),
    {ok, FileInfo} = file:read_file_info(OrigFile),
    NewFile = filename:join([PrivDir, Base]),
    {ok, [{shebang, default},
	  {comment, _},
	  {emu_args, EmuArg},
	  {source, Source}]} =
	escript:extract(OrigFile, []),

    %% Compile the code
    ErlFile = NewFile ++ ".erl",
    ErlCode = list_to_binary(["\n-module(", Base, ").\n",
			      "-export([main/1]).\n\n",
			      Source, "\n\n"]),
    ok = file:write_file(ErlFile, ErlCode),

    %% Compile the code
    {ok, _Mod, BeamCode} =
	compile:file(ErlFile, [binary, debug_info]),

    {NewFile, FileInfo,
     EmuArg, Source,
     ErlFile, ErlCode,
     NewFile ++ ".beam", BeamCode}.

verify_sections(Config, File, FileInfo, Sections) ->
    %% io:format("~p:verify_sections(\n\t~p,\n\t~p,\n\t~p).\n",
    %%           [?MODULE, File, FileInfo, Sections]),

    %% Create
    file:delete(File),
    ok = escript:create(File, Sections),
    ok = file:write_file_info(File, FileInfo),

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
	    run(Config, Dir, InputArgs, [Expected]);
	false ->
	    run_with_opts(Config, Dir, [], InputArgs, [Expected])
    end,

    %% Verify
    {ok, Bin} = escript:create(binary, Sections),
    {ok, Read} = file:read_file(File),
    Bin = Read, % Assert

    Normalized = normalize_sections(Sections),
    {ok, Extracted} = escript:extract(File, []),
    %% io:format("Normalized: ~p\n", [Normalized]),
    %% io:format("Extracted:  ~p\n", [Extracted]),
    ?assertEqual(Normalized, Extracted),
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
                          [{emu_args, _} | _]=Body ->
                              Body;
                          [{modules, _} | _]=Body ->
                              [{emu_args, undefined} | Body];
			  [Body] ->
			      [{emu_args, undefined}, Body]
		      end
		     ];
                 [{emu_args, _}, {modules, _} | _]=Rest2 ->
                     [{comment, undefined} | Rest2];
		 [{emu_args, EmuArgs}, Body] ->
		     [{comment, undefined}, {emu_args, EmuArgs}, Body];
                 [{modules, _} | _]=Body ->
                     [{comment, undefined}, {emu_args, undefined} | Body];
		 [Body] ->
		     [{comment, undefined}, {emu_args, undefined}, Body]
	     end];
        [{modules, _} | _]=Body ->
            [{shebang, undefined}, {comment, undefined}, {emu_args, undefined} |
             Body];
	[Body] ->
	    [{shebang, undefined}, {comment, undefined}, {emu_args, undefined}, Body]
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unicode(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "unicode1",
        [<<"escript: exception error: an error occurred when evaluating"
           " an arithmetic expression\n  in operator  '/'/2\n     "
           "called as <<224,170,170>> / <<224,170,170>>\nExitCode:127">>]),
    run(Config, Dir, "unicode2",
        [<<"escript: exception error: an error occurred when evaluating"
           " an arithmetic expression\n  in operator  '/'/2\n     "
           "called as <<\"\xaa\">> / <<\"\xaa\">>\nExitCode:127">>]),
    run(Config, Dir, "unicode3", [<<"ExitCode:0">>]),
    run(Config, Dir, "unicode4", [<<"ExitCode:0">>]),
    run(Config, Dir, "unicode5", [<<"ExitCode:0">>]),
    run(Config, Dir, "unicode6", [<<"ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overflow(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "arg_overflow",
	[<<"ExitCode:0">>]),
    run(Config, Dir, "linebuf_overflow",
	[<<"ExitCode:0">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% OTP-16006, ERL-992
bad_io_server(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    run(Config, Dir, "bad_io_server",
        [<<"escript: exception error: an error occurred when evaluating"
           " an arithmetic expression\n  in operator  '/'/2\n     "
           "called as '\\x{400}' / 0\nExitCode:127">>]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bypass_unicode_conversion(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Dir = filename:absname(Data),		%Get rid of trailing slash.
    ToNull = case os:type() of
        {win32,_} -> " 1> nul ";
        _ -> " 1> /dev/null "
    end,
    Cmd = fun(Enc) -> "bypass_unicode_conversion "++atom_to_list(Enc)++ToNull end,
    {TimeLatin1, _} = timer:tc(
        fun() -> run(Config, Dir, Cmd(latin1), [<<"ExitCode:0">>]) end),
    {TimeUnicode, _} = timer:tc(
        fun() -> run(Config, Dir, Cmd(unicode), [<<"ExitCode:0">>]) end),
    %% Check that Time(latin1) is about the same as Time(unicode)
    %% Without the bypass, the time difference would be about 5x.
    %% Turns out that the timing might be a bit unstable, so we allow a 2x difference.
    io:format("Time(latin1) = ~p ~~= Time(unicode) = ~p~n", [TimeLatin1, TimeUnicode]),
    true = TimeLatin1 =< TimeUnicode * 2,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Config, Dir, Cmd, Expected) ->
    run_with_opts(Config, Dir, "", Cmd, Expected).

run_with_opts(Config, Dir, Opts, Cmd0, Expected) ->
    [CmdName | _] = string:split(Cmd0, " ", all),
    Cmd = case os:type() of
	      {win32,Wtype} ->
                  %% This case is stolen from os:mk_cmd/2
                  Command = case {os:getenv("COMSPEC"),Wtype} of
                                {false,windows} -> "command.com /c ";
                                {false,_} -> "cmd /c ";
                                {Cspec,_} -> lists:concat([Cspec," /c "])
                            end,
                  Command ++ "escript " ++ Opts ++ " " ++ filename:nativename(Dir) ++ "\\" ++ Cmd0;
	      _ -> "escript " ++ Opts ++ " " ++ Dir ++ "/" ++ Cmd0
	  end,
    do_run(Config, CmdName, Dir, Cmd, Expected).

do_run(Config, CmdName, Dir, Cmd0, Expected0) ->
    StdErrFile = tempnam(Config, CmdName),
    Cmd = Cmd0 ++ " 2> " ++ filename:nativename(StdErrFile),
    io:format("Run: ~ts\n", [Cmd]),
    Expected = iolist_to_binary(expected_output(Expected0, Dir)),

    Env = [{"PATH",Dir++":"++os:getenv("PATH")},
           {"ERL_FLAGS",false},{"ERL_AFLAGS",false}],
    Port = open_port({spawn,Cmd}, [exit_status,eof,in,{env,Env},hide]),
    StdOut = get_data(Port, []),
    receive
	{Port,{exit_status,ExitCode}} ->
	    {ok, StdErr} = file:read_file(StdErrFile),
	    file:delete(StdErrFile),
	    Res = [StdErr, StdOut],
	    Actual = iolist_to_binary([Res,"ExitCode:"++integer_to_list(ExitCode)]),
	    case matches(Expected, Actual) of
		true ->
		    ok;
		false ->
		    io:format("Expected: ~ts\n", [Expected]),
		    io:format("Actual:   ~ts\n", [Actual]),
		    ct:fail(failed)
	    end
    end.

%% Check if Expected and Actual contain the same lines.
%% The lines may occur in different order.
matches(Expected, Actual) ->
    ExpectedLines = string:split(Expected, "\n", all),
    ActualLines = string:split(Actual, "\n", all),
    matches_1(ExpectedLines, ActualLines).

matches_1([], []) -> true;
matches_1([Line | Expected], Actual0) ->
    case delete_first(Line, Actual0) of
	false -> false;
	Actual -> matches_1(Expected, Actual)
    end.

delete_first(X, L) -> delete_first(X, L, []).

delete_first(_X, [], _Acc) -> false;
delete_first(X, [X | Tail], Acc) -> lists:reverse(Acc, Tail);
delete_first(X, [Y | Tail], Acc) -> delete_first(X, Tail, [Y | Acc]).

tempnam(Config, Prefix) ->
    Dir = proplists:get_value(priv_dir, Config),
    File = filename:join(Dir, Prefix ++ ".stderr"),
    case file:delete(File) of
	ok -> ok;
	{error, enoent} -> ok
    end,
    case file:open(File, [write, exclusive, raw]) of
	{ok, IoDev} ->
	    ok = file:close(IoDev),
	    ok = file:delete(File),
	    File;
	{error, Reason} ->
	    io:format("Failed to create ~s: ~p\n", [File, Reason]),
	    ct:fail(failed)
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

