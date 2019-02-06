-module(hipe_testsuite_driver).

-export([create_all_suites/1, run/3]).

-include_lib("kernel/include/file.hrl").

-type testcase()  :: atom().
-type file_type() :: 'device' | 'directory' | 'regular' | 'other'.
-type ext_posix() :: file:posix() | 'badarg'.

-define(suite_suffix, "_SUITE").
-define(data_folder, "_data").
-define(suite_data, ?suite_suffix ++ ?data_folder).

-record(suite, {suitename  :: string(),
		outputfile :: file:io_device(),
		testcases  :: [testcase()]}).

-spec create_all_suites([string()]) -> 'ok'.

create_all_suites(SuitesWithSuiteSuffix) ->
    Suites = get_suites(SuitesWithSuiteSuffix),
    lists:foreach(fun create_suite/1, Suites).

-spec get_suites([string()]) -> [string()].

get_suites(SuitesWithSuiteSuffix) ->
    Prefixes = [suffix(F, ?suite_suffix) || F <- SuitesWithSuiteSuffix],
    [S || {yes, S} <- Prefixes].

suffix(String, Suffix) ->
    case string:split(String, Suffix, trailing) of
	[Prefix,[]] -> {yes, Prefix};
        _ -> no
    end.

-spec file_type(file:filename()) -> {ok, file_type()} | {error, ext_posix()}.

file_type(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FI} -> {ok, FI#file_info.type};
        Error    -> Error
    end.

-spec create_suite(string()) -> 'ok'.

create_suite(SuiteName) ->
    {ok, Cwd} = file:get_cwd(),
    SuiteDirN = filename:join(Cwd, SuiteName ++ ?suite_data),
    OutputFile = generate_suite_file(Cwd, SuiteName),
    generate_suite(SuiteName, OutputFile, SuiteDirN).

generate_suite_file(Cwd, SuiteName) ->
    F =	filename:join(Cwd, SuiteName ++ ?suite_suffix ++ ".erl"),
    case file:open(F, [write]) of
	{ok, IoDevice} -> IoDevice;
	{error, _} = E -> exit({E, F})
    end.

generate_suite(SuiteName, OutputFile, SuiteDirN) ->
    TestCases = list_testcases(SuiteDirN),
    Suite = #suite{suitename = SuiteName, outputfile = OutputFile,
		   testcases = TestCases},
    write_suite(Suite),
    file:close(OutputFile).

list_testcases(Dirname) ->
    {ok, Files} = list_dir(Dirname, ".erl", true),
    [list_to_atom(filename:basename(F, ".erl")) || F <- Files].

-spec list_dir(file:filename(), string(), boolean()) ->
                      {error, ext_posix()} | {ok, [file:filename()]}.

list_dir(Dir, Extension, Dirs) ->
    case file:list_dir(Dir) of
        {error, _} = Error -> Error;
        {ok, Filenames} ->
            FullFilenames = [filename:join(Dir, F) || F <- Filenames],
            Matches1 = case Dirs of
                           true ->
                               [F || F <- FullFilenames,
                                     file_type(F) =:= {ok, 'directory'}];
                           false -> []
                       end,
            Matches2 = [F || F <- FullFilenames,
                             file_type(F) =:= {ok, 'regular'},
                             filename:extension(F) =:= Extension],
            {ok, lists:sort(Matches1 ++ Matches2)}
    end.

write_suite(Suite) ->
    write_header(Suite),
    write_testcases(Suite).

write_header(#suite{suitename = SuiteName, outputfile = OutputFile,
		    testcases = TestCases}) ->
    Exports = format_export(TestCases),
    TimeLimit = 6,	%% with 1, 2, or 3 it fails on some slow machines...
    io:format(OutputFile,
	      "%% ATTENTION!\n"
	      "%% This is an automatically generated file. Do not edit.\n\n"
	      "-module(~s).\n\n"
	      "-export([suite/0, init_per_suite/0, init_per_suite/1,\n"
	      "         end_per_suite/1, all/0]).\n"
	      "~s\n\n"
	      "-include_lib(\"common_test/include/ct.hrl\").\n\n"
	      "suite() ->\n"
	      "  [{timetrap, {minutes, ~w}}].\n\n"
	      "init_per_suite() ->\n"
	      "  [].\n\n"
	      "init_per_suite(Config) ->\n"
	      "  case erlang:system_info(hipe_architecture) of\n"
	      "    undefined -> {skip, \"HiPE not available or enabled\"};\n"
	      "    _ -> Config\n"
	      "  end.\n\n"
	      "end_per_suite(_Config) ->\n"
	      "  ok.\n\n"
	      "all() ->\n"
	      "  ~p.\n\n"
	      "test(Config, TestCase) ->\n"
	      "  Dir = ?config(data_dir, Config),\n"
	      "  OutDir = ?config(priv_dir, Config),\n"
	      "  hipe_testsuite_driver:run(TestCase, Dir, OutDir)."
	      "\n\n",
	      [SuiteName ++ ?suite_suffix, Exports, TimeLimit, TestCases]).

format_export(TestCases) ->
    TL = [list_to_atom(atom_to_list(N)++"/1") || N <- TestCases],
    TestCaseString = io_lib:format("-export(~p).", [TL]),
    strip_quotes(lists:flatten(TestCaseString), []).

strip_quotes([], Result) ->
    lists:reverse(Result);
strip_quotes([$' |Rest], Result) ->
    strip_quotes(Rest, Result);
strip_quotes([$\, |Rest], Result) ->
    strip_quotes(Rest, [$\ , $\, |Result]);
strip_quotes([C|Rest], Result) ->
    strip_quotes(Rest, [C|Result]).

write_testcases(#suite{outputfile = OutputFile, testcases = TestCases}) ->
    lists:foreach(fun (T) -> write_testcase(OutputFile, T) end, TestCases).

write_testcase(OutputFile, TestCase) ->
    io:format(OutputFile,
	      "~p(Config) ->\n"
	      "  test(Config, ~p).\n\n",
	      [TestCase, TestCase]).

-spec run(atom(), string(), string()) -> 'ok'.

run(TestCase, Dir, _OutDir) ->
    F = filename:join(Dir, atom_to_list(TestCase) ++ ".erl"),
    {ok, TestCase} = compile:file(F),
    ok = try TestCase:prepare_for_test() catch _:_ -> ok end,
    %% DataFiles = try TestCase:datafiles() catch _:_ -> [] end,
    %% lists:foreach(fun (DF) ->
    %% 			  Src = filename:join(Dir, DF),
    %% 			  Dst = filename:join(OutDir, DF),
    %% 			  {ok, _} = file:copy(Src, Dst)
    %% 		  end, DataFiles),
    %% try
    ok = TestCase:test(),
    HiPEOpts0 = try TestCase:hipe_options() catch error:undef -> [] end,
    HiPEOpts = HiPEOpts0 ++ hipe_options(),
    {ok, TestCase} = hipe:c(TestCase, HiPEOpts),
    ok = TestCase:test(),
    {ok, TestCase} = hipe:c(TestCase, [o1|HiPEOpts]),
    ok = TestCase:test(),
    {ok, TestCase} = hipe:c(TestCase, [o0|HiPEOpts]),
    ok = TestCase:test(),
    ToLLVM = try TestCase:to_llvm() catch error:undef -> true end,
    case ToLLVM andalso hipe:llvm_support_available() of
	true ->
	    {ok, TestCase} = hipe:c(TestCase, [to_llvm|HiPEOpts]),
	    ok = TestCase:test();
	false -> ok
    end.
    %% after
    %% 	lists:foreach(fun (DF) -> ok end, % = file:delete(DF) end,
    %% 		      [filename:join(OutDir, D) || D <- DataFiles])
    %% end.

hipe_options() ->
    [verify_gcsafe].
