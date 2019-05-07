%%% File        : dialyzer_common.erl
%%% Author      : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Generator and common infrastructure for simple dialyzer
%%%               test suites (some options, some input files or directories
%%%               and the relevant results).
%%% Created     : 11 Jun 2010 by Stavros Aronis <stavros@enjoy>

-module(dialyzer_common).

-export([check_plt/1, check/4, create_all_suites/0, new_tests/2, plt_file/1]).

-include_lib("kernel/include/file.hrl").

-define(suite_suffix, "_SUITE").
-define(data_folder, "_data").
-define(suite_data, ?suite_suffix ++ ?data_folder).
-define(erlang_extension, ".erl").
-define(output_file_mode, write).
-define(dialyzer_option_file, "dialyzer_options").
-define(input_files_directory, "src").
-define(result_files_directory, "results").
-define(plt_filename,"dialyzer_plt").
-define(home_plt_filename,".dialyzer_plt").
-define(plt_lockfile,"plt_lock").
-define(required_modules, [erts, kernel, stdlib]).

-record(suite, {suitename  :: string(),
		outputfile :: file:io_device(),
		options    :: options(),
		testcases  :: [testcase()]}).

-record(options, {time_limit       =  1 :: integer(),
		  dialyzer_options = [] :: dialyzer:dial_options()}).

-type options() :: #options{}.
-type testcase() :: {atom(), 'file' | 'dir'}.

-spec check_plt(string()) -> ok.

check_plt(OutDir) ->
    io:format("Checking plt:"),
    PltFilename = plt_file(OutDir),
    case file:read_file_info(PltFilename) of
	{ok, _} -> dialyzer_check_plt(PltFilename);
	{error, _ } ->
	    io:format("No plt found in test run directory!"),
	    PltLockFile = filename:join(OutDir, ?plt_lockfile),
	    case file:read_file_info(PltLockFile) of 
		{ok, _} ->
		    explain_fail_with_lock(),
		    fail;
		{error, _} ->
		    io:format("Locking plt generation."),
		    case file:open(PltLockFile,[?output_file_mode]) of
			{ok, OutFile} ->
			    io:format(OutFile,"Locking plt generation.",[]),
			    file:close(OutFile);
			{error, Reason} ->
			    io:format("Couldn't write lock file ~p.",[Reason]),
			    fail
		    end,
		    obtain_plt(PltFilename)
	    end
    end.

-spec plt_file(string()) -> string().

plt_file(OutDir) ->
    filename:join(OutDir, ?plt_filename).

dialyzer_check_plt(PltFilename) ->
    try dialyzer:run([{analysis_type, plt_check},
		      {init_plt, PltFilename}]) of
	[] -> ok
    catch
	Class:Info ->
	    io:format("Failed. The error was: ~w\n~p",[Class, Info]),
	    io:format("A previously run dialyzer suite failed to generate"
		      " a correct plt."),
	    fail
    end.

explain_fail_with_lock() ->
    io:format("Some other suite started creating a plt. It might not have"
	      " finished (Dialyzer's suites shouldn't run in parallel), or"
	      " it reached timeout and was killed (in which case"
	      " plt_timeout, defined in dialyzer_test_constants.hrl"
	      " should be increased), or it failed.").

obtain_plt(PltFilename) ->
    io:format("Obtaining plt:"),
    HomeDir = os:getenv("HOME"),
    HomePlt = filename:join(HomeDir, ?home_plt_filename),
    io:format("Will try to use ~s as a starting point and add otp apps ~w.",
	      [HomePlt, ?required_modules]),
    try dialyzer:run([{analysis_type, plt_add},
		      {apps, ?required_modules},
		      {output_plt, PltFilename},
		      {init_plt, HomePlt}]) of
	[] ->
	    io:format("Successfully added everything!"),
	    ok
    catch
	Class:Reason ->
	    io:format("Failed. The error was: ~w\n~p",[Class, Reason]),
	    build_plt(PltFilename)
    end.

build_plt(PltFilename) ->
    io:format("Building plt from scratch:"),
    try dialyzer:run([{analysis_type, plt_build},
		      {apps, ?required_modules},
		      {output_plt, PltFilename}]) of
	[] ->
	    io:format("Successfully created plt!"),
	    ok
    catch
	Class:Reason ->
	    io:format("Failed. The error was: ~w\n~p",[Class, Reason]),
	    fail
    end.

-spec check(atom(), dialyzer:dial_options(), string(), string()) ->
		   'same' | {differ, [term()]}.

check(TestCase, Opts, Dir, OutDir) ->
    PltFilename = plt_file(OutDir),
    SrcDir = filename:join(Dir, ?input_files_directory),
    ResDir = filename:join(Dir, ?result_files_directory),
    Filename = filename:join(SrcDir, atom_to_list(TestCase)),
    Files =
	case file_utils:file_type(Filename) of
	    {ok, 'directory'} ->
		{ok, ListFiles} = file_utils:list_dir(Filename, ".erl",
						      false),
		ListFiles;
	    {error, _} ->
		FilenameErl = Filename ++ ".erl",
		case file_utils:file_type(FilenameErl) of
		    {ok, 'regular'} -> [FilenameErl]
		end
	end,
    ResFile = atom_to_list(TestCase),
    NewResFile = filename:join(OutDir, ResFile),
    OldResFile = filename:join(ResDir, ResFile),
    ProperOpts = fix_options(Opts, Dir),
    try dialyzer:run([{files, Files},{from, src_code},{init_plt, PltFilename},
		      {check_plt, false}|ProperOpts]) of
	RawWarns ->
	    Warns = lists:sort([dialyzer:format_warning(W, ProperOpts) ||
                                   W <- RawWarns]),
	    case Warns of
		[] -> ok;
		_  ->
		    case file:open(NewResFile,[?output_file_mode]) of
			{ok, OutFile} ->
			    io:format(OutFile,"\n~s",[Warns]),
			    file:close(OutFile);
			Other -> erlang:error(Other)
		    end
	    end,
	    case file_utils:diff(NewResFile, OldResFile) of
		'same' -> file:delete(NewResFile),
			  'same';
		Any    -> escape_strings(Any)
	    end
    catch
	Kind:Error -> {'dialyzer crashed', Kind, Error}
    end.

fix_options(Opts, Dir) ->
    fix_options(Opts, Dir, []).

fix_options([], _Dir, Acc) ->
    Acc;
fix_options([{pa, Path} | Rest], Dir, Acc) ->
    case code:add_patha(filename:join(Dir, Path)) of
	true       -> fix_options(Rest, Dir, Acc);
	{error, _} -> erlang:error("Bad directory for pa: " ++ Path)
    end;
fix_options([{DirOption, RelativeDirs} | Rest], Dir, Acc) 
  when DirOption =:= include_dirs ;
       DirOption =:= files_rec ;
       DirOption =:= files ->
    ProperRelativeDirs = [filename:join(Dir,RDir) || RDir <- RelativeDirs],
    fix_options(Rest, Dir, [{include_dirs, ProperRelativeDirs} | Acc]);
fix_options([Opt | Rest], Dir, Acc) ->
    fix_options(Rest, Dir, [Opt | Acc]).

-spec new_tests(string(), [atom()]) -> [atom()].

new_tests(Dirname, DeclaredTestcases) ->
    SrcDir = filename:join(Dirname, ?input_files_directory),
    get_testcases(SrcDir) -- DeclaredTestcases.

get_testcases(Dirname) ->
    {ok, Files} = file_utils:list_dir(Dirname, ".erl", true),
    [list_to_atom(filename:basename(F,".erl")) || F <-Files].

-spec create_all_suites() -> 'ok'.

create_all_suites() ->
    {ok, Cwd} = file:get_cwd(),
    Suites = get_suites(Cwd),
    lists:foreach(fun create_suite/1, Suites).

escape_strings({differ,List}) ->
    Map = fun({T,L,S}) -> {T,L,xmerl_lib:export_text(S)} end,
    {differ, lists:keysort(3, lists:map(Map, List))}.

-spec get_suites(file:filename()) -> [string()].

get_suites(Dir) ->
    case file:list_dir(Dir) of
	{error, _} -> [];
	{ok, Filenames} ->
	    FullFilenames = [filename:join(Dir, F) || F <-Filenames ],
	    Dirs = [suffix(filename:basename(F), ?suite_data) ||
		       F <- FullFilenames,
		       file_utils:file_type(F) =:= {ok, 'directory'}],
	    [S || {yes, S} <- Dirs]
    end.

suffix(String, Suffix) ->
    case string:split(String, Suffix, trailing) of
	[Prefix,[]] -> {yes, Prefix};
        _ -> no
    end.

-spec create_suite(string()) -> 'ok'.

create_suite(SuiteName) ->
    {ok, Cwd} = file:get_cwd(),
    SuiteDirN = generate_suite_dir_from_name(Cwd, SuiteName),
    OutputFile = generate_suite_file(Cwd, SuiteName),
    {OptionsFileN, InputDirN} = check_neccessary_files(SuiteDirN),
    generate_suite(SuiteName, OutputFile, OptionsFileN, InputDirN).

generate_suite_dir_from_name(Cwd, SuiteName) ->
    filename:join(Cwd, SuiteName ++ ?suite_data).

generate_suite_file(Cwd, SuiteName) ->
    OutputFilename =
	filename:join(Cwd, SuiteName ++ ?suite_suffix ++ ?erlang_extension),
    case file:open(OutputFilename, [?output_file_mode]) of
	{ok, IoDevice} -> IoDevice;
	{error, _} = E -> exit({E, OutputFilename})
    end.

check_neccessary_files(SuiteDirN) ->
    InputDirN = filename:join(SuiteDirN, ?input_files_directory),
    check_file_exists(InputDirN, directory),
    OptionsFileN = filename:join(SuiteDirN, ?dialyzer_option_file),
    check_file_exists(OptionsFileN, regular),
    {OptionsFileN, InputDirN}.

check_file_exists(Filename, Type) ->
    case file:read_file_info(Filename) of
	{ok, FileInfo} ->
	    case FileInfo#file_info.type of
		Type -> ok;
		Else -> exit({error, {wrong_input_file_type, Else}})
	    end;
	{error, _} = E -> exit({E, Filename, Type})
    end.

generate_suite(SuiteName, OutputFile, OptionsFileN, InputDirN) ->
    Options = read_options(OptionsFileN),
    TestCases = list_testcases(InputDirN),
    Suite = #suite{suitename = SuiteName, outputfile = OutputFile,
		   options = Options, testcases = TestCases},
    write_suite(Suite),
    file:close(OutputFile).

read_options(OptionsFileN) ->
    case file:consult(OptionsFileN) of
	{ok, Opts} -> read_options(Opts, #options{});
	_ = E      -> exit({error, {incorrect_options_file, E}})
    end.

read_options([List], Options) when is_list(List) ->
    read_options(List, Options);
read_options([], Options) ->
    Options;
read_options([{time_limit, TimeLimit}|Opts], Options) ->
    read_options(Opts, Options#options{time_limit = TimeLimit});
read_options([{dialyzer_options, DialyzerOptions}|Opts], Options) ->
    read_options(Opts, Options#options{dialyzer_options = DialyzerOptions}).

list_testcases(Dirname) ->
    {ok, Files} = file_utils:list_dir(Dirname, ".erl", true),
    [list_to_atom(filename:basename(F,".erl")) || F <-Files].

write_suite(Suite) ->
    write_header(Suite),
    write_consistency(Suite),
    write_testcases(Suite).

write_header(#suite{suitename = SuiteName, outputfile = OutputFile,
		    options = Options, testcases = TestCases}) ->
    Test_Plus_Consistency =
	[list_to_atom(SuiteName ++ ?suite_suffix ++ "_consistency")|TestCases],
    Exports = format_export(Test_Plus_Consistency),
    TimeLimit = Options#options.time_limit,
    DialyzerOptions = Options#options.dialyzer_options,
    io:format(OutputFile,
	      "%% ATTENTION!\n"
	      "%% This is an automatically generated file. Do not edit.\n"
	      "%% Use './remake' script to refresh it if needed.\n"
	      "%% All Dialyzer options should be defined in dialyzer_options\n"
	      "%% file.\n\n"
	      "-module(~s).\n\n"
	      "-include_lib(\"common_test/include/ct.hrl\").\n"
	      "-include(\"dialyzer_test_constants.hrl\").\n\n"
	      "-export([suite/0, init_per_suite/0, init_per_suite/1,\n"
	      "         end_per_suite/1, all/0]).\n"
	      "~s\n\n"
	      "suite() ->\n"
	      "  [{timetrap, {minutes, ~w}}].\n\n"
	      "init_per_suite() ->\n"
	      "  [{timetrap, ?plt_timeout}].\n"
	      "init_per_suite(Config) ->\n"
	      "  OutDir = ?config(priv_dir, Config),\n"
	      "  case dialyzer_common:check_plt(OutDir) of\n"
	      "    fail -> {skip, \"Plt creation/check failed.\"};\n"
	      "    ok -> [{dialyzer_options, ~p}|Config]\n"
	      "  end.\n\n"
	      "end_per_suite(_Config) ->\n"
	      "  ok.\n\n"
	      "all() ->\n"
	      "  ~p.\n\n"
	      "dialyze(Config, TestCase) ->\n"
	      "  Opts = ?config(dialyzer_options, Config),\n"
	      "  Dir = ?config(data_dir, Config),\n"
	      "  OutDir = ?config(priv_dir, Config),\n"
	      "  dialyzer_common:check(TestCase, Opts, Dir, OutDir)."
	      "\n\n"
	      ,[SuiteName ++ ?suite_suffix, Exports, TimeLimit,
		DialyzerOptions, Test_Plus_Consistency]).

format_export(TestCases) ->
    TestCasesArity =
	[list_to_atom(atom_to_list(N)++"/1") || N <- TestCases],
    TestCaseString = io_lib:format("-export(~p).", [TestCasesArity]),
    strip_quotes(lists:flatten(TestCaseString),[]).

strip_quotes([], Result) ->
    lists:reverse(Result);
strip_quotes([$' |Rest], Result) ->
    strip_quotes(Rest, Result);
strip_quotes([$\, |Rest], Result) ->
    strip_quotes(Rest, [$\ , $\, |Result]);
strip_quotes([C|Rest], Result) ->
    strip_quotes(Rest, [C|Result]).

write_consistency(#suite{suitename = SuiteName, outputfile = OutputFile}) ->
    write_consistency(SuiteName, OutputFile).

write_consistency(SuiteName, OutputFile) ->
    io:format(OutputFile,
	      "~s_consistency(Config) ->\n"
	      "  Dir = ?config(data_dir, Config),\n"
	      "  case dialyzer_common:new_tests(Dir, all()) of\n"
	      "    []  -> ok;\n"
	      "    New -> ct:fail({missing_tests,New})\n"
	      "  end.\n\n",
	      [SuiteName ++ ?suite_suffix]).

write_testcases(#suite{outputfile = OutputFile, testcases = TestCases}) ->
    write_testcases(OutputFile, TestCases).

write_testcases(OutputFile, [TestCase| Rest]) ->
    io:format(OutputFile,
	      "~p(Config) ->\n"
	      "  case dialyze(Config, ~p) of\n"
	      "    'same' -> 'same';\n"
	      "    Error  -> ct:fail(Error)\n"
              "  end.\n\n",
	      [TestCase, TestCase]),
    write_testcases(OutputFile, Rest);
write_testcases(_OutputFile, []) ->
    ok.
