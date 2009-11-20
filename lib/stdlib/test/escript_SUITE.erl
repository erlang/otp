%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
	 all/1,
	 init_per_testcase/2,
	 fin_per_testcase/2,
	 basic/1,
	 errors/1, 
	 strange_name/1,
	 emulator_flags/1,
	 module_script/1,
	 beam_script/1,
	 archive_script/1,
	 epp/1
	]).

-include("test_server.hrl").

all(suite) ->
    [
     basic,
     errors,
     strange_name,
     emulator_flags,
     module_script,
     beam_script,
     archive_script,
     epp
    ].

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Case, Config) ->
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
    ?line run(Dir, "-s", "factorial_warning",
	      [data_dir,<<"factorial_warning:12: Warning: function bar/0 is unused\nExitCode:0">>]),
    ?line run(Dir, "-s -i", "factorial_warning",
	      [data_dir,<<"factorial_warning:12: Warning: function bar/0 is unused\nExitCode:0">>]),
    ?line run(Dir, "-c -s", "factorial_warning",
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
    ?line run(Dir, "-s", "lint_error",
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
    ErlCode = ["-module(", Base, ").\n",
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
    
    ?line run(Dir, NoArgsBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n"
		"nostick:[]\n"
		"mnesia:[]\n"
		"ERL_FLAGS=false\n"
		"unknown:[]\n"
		"ExitCode:0">>]),

    ?line run(Dir, "", NoArgsBase ++  " -arg1 arg2 arg3",
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
   
    ?line run(Dir, "", NoArgsBase2 ++  " -arg1 arg2 arg3",
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
    
    ?line run(Dir, "", NoArgsBase3 ++  " -arg1 arg2 arg3",
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
			       ["-module(", Base, ").\n",
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

    ?line run(Dir, "", NoArgsBase ++  " -arg1 arg2 arg3",
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

    ?line run(Dir, "", NoArgsBase2 ++  " -arg1 arg2 arg3",
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

    ?line run(Dir, "", NoArgsBase3 ++  " -arg1 arg2 arg3",
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
%% (with .app and .beam files and ) and the escript header.

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

    ?line run(PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
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
   
    ?line run(PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
	      [<<"main:[\"-arg1\",\"arg2\",\"arg3\"]\n" 
		"dict:[]\n"
		"dummy:[]\n"
		"priv:{ok,<<\"Some private data...\\n\">>}\n"
		"ExitCode:0">>]),
    ?line ok = file:rename(MainScript, MainScript ++ "_without_shebang"),
    
    %% Plain archive without header (no flags)
    
    ?line ok = file:write_file(MainScript, [ArchiveBin]),
    ?line ok = file:write_file_info(MainScript, OrigFI),    
   
    ?line run(PrivDir, "", MainBase ++  " -arg1 arg2 arg3",
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

run(Dir, Cmd0, Expected0) ->
    Expected = iolist_to_binary(expected_output(Expected0, Dir)),
    Cmd = case os:type() of
	      {win32,_} -> "escript " ++ filename:nativename(Dir) ++ "\\" ++ Cmd0;
	      _ -> Cmd0
	  end,
    do_run(Dir, Cmd, Expected).

run(Dir, Opts, Cmd0, Expected) ->
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

