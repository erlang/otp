%%% File    : dialyzer_test_suite_generator.erl
%%% Author  : Stavros Aronis <stavros@enjoy>
%%% Description : Generator for simple dialyzer test suites (some options,
%%%               some input files or directories and the relevant results).
%%% Created : 11 Jun 2010 by Stavros Aronis <stavros@enjoy>

-module(generator).

-export([suite/1]).

-include_lib("kernel/include/file.hrl").

-define(suite_suffix, "_tests_SUITE").
-define(data_folder, "_data").
-define(erlang_extension, ".erl").
-define(output_file_mode, write).
-define(dialyzer_option_file, "dialyzer_options").
-define(input_files_directory, "src").
-define(result_files_directory, "result").

-record(suite, {suitename  :: string(),
		outputfile :: file:io_device(),
		options    :: options(),
		testcases  :: [testcase()]}).

-record(options, {time_limit       =  1 :: integer(),
		  dialyzer_options = [] :: [term()]}).

-type options() :: #options{}.
-type testcase() :: {atom(), 'file' | 'dir'}.

-spec suite(string()) -> 'ok'.

suite(SuiteName) ->
    {ok, Cwd} = file:get_cwd(),
    SuiteDirN = generate_suite_dir_from_name(Cwd, SuiteName),
    OutputFile = generate_suite_file(Cwd, SuiteName),
    {OptionsFileN, InputDirN} = check_neccessary_files(SuiteDirN),
    generate_suite(SuiteName, OutputFile, OptionsFileN, InputDirN).

generate_suite_dir_from_name(Cwd, SuiteName) ->
    filename:join(Cwd, SuiteName ++ ?suite_suffix ++ ?data_folder).

generate_suite_file(Cwd, SuiteName) ->
    OutputFilename =
	filename:join(Cwd, SuiteName ++ ?suite_suffix ++ ?erlang_extension),
    case file:open(OutputFilename, [?output_file_mode]) of
	{ok, IoDevice} -> IoDevice;
	{error, _} = E -> exit(E)
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
	{error, _} = E -> exit(E)
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

list_testcases(InputDirN) ->
    {ok, PartialFilenames} = file:list_dir(InputDirN),
    Filenames = [filename:join(InputDirN, F) || F <- PartialFilenames],
    SafeFilenames = [F || F <- Filenames, safe_extension(F)],
    lists:sort(lists:map(fun(X) -> map_testcase(X) end, SafeFilenames)).

safe_extension(Filename) ->
    Extension = filename:extension(Filename),
    Extension =:= ".erl" orelse Extension =:= "".

map_testcase(Filename) ->
    TestCase = list_to_atom(filename:basename(Filename, ?erlang_extension)),
    {ok, FileInfo} = file:read_file_info(Filename),
    case FileInfo#file_info.type of
	directory -> {TestCase, dir};
	regular   -> {TestCase, file}
    end.

write_suite(Suite) ->
    write_header(Suite),
    write_testcases(Suite),
    write_footer(Suite).

write_header(#suite{suitename = SuiteName, outputfile = OutputFile,
		    options = Options, testcases = TestCases}) ->
    TestCaseNames = [N || {N, _} <- TestCases],
    Exports = format_export(TestCaseNames),
    TimeLimit = Options#options.time_limit,
    DialyzerOptions = Options#options.dialyzer_options,
    io:format(OutputFile,
	      "-module(~s).\n\n"
	      "-include_lib(\"test_server/include/test_server.hrl\").\n\n"
	      "-export([all/0, groups/0, init_per_group/2, end_per_group/2,\n"
	      "         init_per_testcase/2, fin_per_testcase/2]).\n\n"
	      "~s\n\n"
	      "-define(default_timeout, ?t:minutes(~p)).\n"
	      "-define(dialyzer_options, ?config(dialyzer_options, Config)).\n"
	      "-define(datadir, ?config(data_dir, Config)).\n"
	      "-define(privdir, ?config(priv_dir, Config)).\n\n"
	      "groups() -> [].\n\n"
	      "init_per_group(_GroupName, Config) -> Config.\n\n"
	      "end_per_group(_GroupName, Config) -> Config.\n\n"
	      "init_per_testcase(_Case, Config) ->\n"
	      "    ?line Dog = ?t:timetrap(?default_timeout),\n"
	      "    [{dialyzer_options, ~p}, {watchdog, Dog} | Config].\n\n"
	      "fin_per_testcase(_Case, _Config) ->\n"
	      "    Dog = ?config(watchdog, _Config),\n"
	      "    ?t:timetrap_cancel(Dog),\n"
	      "    ok.\n\n"
	      "all() ->\n"
	      "    ~p.\n\n"
	      ,[SuiteName ++ ?suite_suffix, Exports, TimeLimit,
		DialyzerOptions, TestCaseNames]).

format_export(TestCaseNames) ->
    TestCaseNamesArity = [list_to_atom(atom_to_list(N)++"/1") ||
			     N <- TestCaseNames],
    TestCaseString = io_lib:format("-export(~p).", [TestCaseNamesArity]),
    strip_quotes(lists:flatten(TestCaseString),[]).

strip_quotes([], Result) ->
    lists:reverse(Result);
strip_quotes([$' |Rest], Result) ->
    strip_quotes(Rest, Result);
strip_quotes([$\, |Rest], Result) ->
    strip_quotes(Rest, [$\ , $\, |Result]);
strip_quotes([C|Rest], Result) ->
    strip_quotes(Rest, [C|Result]).

write_testcases(#suite{outputfile = OutputFile, testcases = TestCases}) ->
    write_testcases(OutputFile, TestCases).

write_testcases(OutputFile, [{TestCase, Kind}|TestCases]) ->
    io:format(OutputFile,
	      "~p(Config) when is_list(Config) ->\n"
	      "    ?line run(Config, {~p, ~p}),\n"
	      "    ok.\n\n"
	      ,[TestCase, TestCase, Kind]),
    write_testcases(OutputFile, TestCases);
write_testcases(_OutputFile, []) ->
    ok.

write_footer(#suite{outputfile = OutputFile}) ->
    io:format(OutputFile,
	      "run(Config, TestCase) ->\n"
	      "    case run_test(Config, TestCase) of\n"
	      "        ok -> ok;\n"
	      "        {fail, Reason} ->\n"
	      "            ?t:format(\"~~s\",[Reason]),\n"
	      "            fail()\n"
	      "    end.\n\n"
	      "run_test(Config, {TestCase, Kind}) ->\n"
	      "    Dog = ?config(watchdog, Config),\n"
	      "    Options = ?dialyzer_options,\n"
	      "    Dir = ?datadir,\n"
	      "    OutDir = ?privdir,\n"
	      "    case dialyzer_test:dialyzer_test(Options, TestCase, Kind,\n"
	      "                                     Dir, OutDir, Dog) of\n"
	      "        same -> ok;\n"
	      "        {differ, DiffList} ->\n"
	      "            {fail,\n"
	      "               io_lib:format(\"\\nTest ~~p failed:\\n~~p\\n\",\n"
	      "                            [TestCase, DiffList])}\n"
	      "    end.\n\n"
	      "fail() ->\n"
	      "    io:format(\"failed\\n\"),\n"
	      "    ?t:fail().\n",[]).
