%% -*- erlang-indent-level: 2 -*-
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

-module(dialyzer_cl_parse).

-export([start/0, get_lib_dir/1]).
-export([collect_args/1]).	% used also by typer

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------

-type dial_cl_parse_ret() :: {'check_init', #options{}}
                           | {'plt_info', #options{}}
                           | {'cl', #options{}}
                           | {'gui', #options{}}
                           | {'error', string()}.

-type deep_string() :: string() | [deep_string()].

%%-----------------------------------------------------------------------

-spec start() -> dial_cl_parse_ret().

start() ->
  init(),
  Args = init:get_plain_arguments(),
  try
    Ret = cl(Args),
    Ret
  catch
    throw:{dialyzer_cl_parse_error, Msg} -> {error, Msg};
    _:R:S ->
      Msg = io_lib:format("~tp\n~tp\n", [R, S]),
      {error, lists:flatten(Msg)}
  end.

cl(["--add_to_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_add),
  cl(T);
cl(["--apps"|T]) ->
  T1 = get_lib_dir(T),
  {Args, T2} = collect_args(T1),
  append_var(dialyzer_options_files_rec, Args),
  cl(T2);
cl(["--build_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_build),
  cl(T);
cl(["--check_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_check),
  cl(T);
cl(["-n"|T]) ->
  cl(["--no_check_plt"|T]);
cl(["--no_check_plt"|T]) ->
  put(dialyzer_options_check_plt, false),
  cl(T);
cl(["-nn"|T]) ->
  cl(["--no_native"|T]);
cl(["--no_native"|T]) ->
  put(dialyzer_options_native, false),
  cl(T);
cl(["--no_native_cache"|T]) ->
  put(dialyzer_options_native_cache, false),
  cl(T);
cl(["--plt_info"|T]) ->
  put(dialyzer_options_analysis_type, plt_info),
  cl(T);
cl(["--get_warnings"|T]) ->
  put(dialyzer_options_get_warnings, true),
  cl(T);
cl(["-D"|_]) ->
  cl_error("No defines specified after -D");
cl(["-D"++Define|T]) ->
  Def = re:split(Define, "=", [{return, list}, unicode]),
  append_defines(Def),
  cl(T);
cl(["-h"|_]) ->
  help_message();
cl(["--help"|_]) ->
  help_message();
cl(["-I"]) ->
  cl_error("no include directory specified after -I");
cl(["-I", Dir|T]) ->
  append_include(Dir),
  cl(T);
cl(["-I"++Dir|T]) ->
  append_include(Dir),
  cl(T);
cl(["-c"++_|T]) ->
  NewTail = command_line(T),
  cl(NewTail);
cl(["-r"++_|T0]) ->
  {Args, T} = collect_args(T0),
  append_var(dialyzer_options_files_rec, Args),
  cl(T);
cl(["--remove_from_plt"|T]) ->
  put(dialyzer_options_analysis_type, plt_remove),
  cl(T);
cl(["--com"++_|T]) ->
  NewTail = command_line(T),
  cl(NewTail);
cl(["--output"]) ->
  cl_error("No outfile specified");
cl(["-o"]) ->
  cl_error("No outfile specified");
cl(["--output",Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["--output_plt"]) ->
  cl_error("No outfile specified for --output_plt");
cl(["--output_plt",Output|T]) ->
  put(dialyzer_output_plt, Output),
  cl(T);
cl(["-o", Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["-o"++Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["--raw"|T]) ->
  put(dialyzer_output_format, raw),
  cl(T);
cl(["--fullpath"|T]) ->
  put(dialyzer_filename_opt, fullpath),
  cl(T);
cl(["--no_indentation"|T]) ->
  put(dialyzer_indent_opt, false),
  cl(T);
cl(["-pa", Path|T]) ->
  case code:add_patha(Path) of
    true -> cl(T);
    {error, _} -> cl_error("Bad directory for -pa: " ++ Path)
  end;
cl(["--plt"]) ->
  error("No plt specified for --plt");
cl(["--plt", PLT|T]) ->
  put(dialyzer_init_plts, [PLT]),
  cl(T);
cl(["--plts"]) ->
  error("No plts specified for --plts");
cl(["--plts"|T]) ->
  {PLTs, NewT} = get_plts(T, []),
  put(dialyzer_init_plts, PLTs),
  cl(NewT);
cl(["-q"|T]) ->
  put(dialyzer_options_report_mode, quiet),
  cl(T);
cl(["--quiet"|T]) ->
  put(dialyzer_options_report_mode, quiet),
  cl(T);
cl(["--src"|T]) ->
  put(dialyzer_options_from, src_code),
  cl(T);
cl(["--no_spec"|T]) ->
  put(dialyzer_options_use_contracts, false),
  cl(T);
cl(["--statistics"|T]) ->
  put(dialyzer_timing, true),
  cl(T);
cl(["--resources"|T]) ->
  put(dialyzer_options_report_mode, quiet),
  put(dialyzer_timing, debug),
  cl(T);
cl(["-v"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--version"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--verbose"|T]) ->
  put(dialyzer_options_report_mode, verbose),
  cl(T);
cl(["-W"|_]) ->
  cl_error("-W given without warning");
cl(["-Whelp"|_]) ->
  help_warnings();
cl(["-W"++Warn|T]) ->
  append_var(dialyzer_warnings, [list_to_atom(Warn)]),
  cl(T);
cl(["--dump_callgraph"]) ->
  cl_error("No outfile specified for --dump_callgraph");
cl(["--dump_callgraph", File|T]) ->
  put(dialyzer_callgraph_file, File),
  cl(T);
cl(["--gui"|T]) ->
  put(dialyzer_options_mode, gui),
  cl(T);
cl(["--solver", Solver|T]) -> % not documented
  append_var(dialyzer_solvers, [list_to_atom(Solver)]),
  cl(T);
cl([H|_] = L) ->
  case filelib:is_file(H) orelse filelib:is_dir(H) of
    true ->
      NewTail = command_line(L),
      cl(NewTail);
    false ->
      cl_error("Unknown option: " ++ H)
  end;
cl([]) ->
  {RetTag, Opts} =
    case get(dialyzer_options_analysis_type) =:= plt_info of
      true ->
	put(dialyzer_options_analysis_type, plt_check),
	{plt_info, cl_options()};
      false ->
	case get(dialyzer_options_mode) of
	  gui -> {gui, common_options()};
	  cl ->
	    case get(dialyzer_options_analysis_type) =:= plt_check of
	      true  -> {check_init, cl_options()};
	      false -> {cl, cl_options()}
	    end
	end
    end,
  case dialyzer_options:build(Opts) of
    {error, Msg} -> cl_error(Msg);
    OptsRecord -> {RetTag, OptsRecord}
  end.

%%-----------------------------------------------------------------------

command_line(T0) ->
  {Args, T} = collect_args(T0),
  append_var(dialyzer_options_files, Args),
  %% if all files specified are ".erl" files, set the 'src' flag automatically
  case lists:all(fun(F) -> filename:extension(F) =:= ".erl" end, Args) of
    true -> put(dialyzer_options_from, src_code);
    false -> ok
  end,
  T.

-spec cl_error(deep_string()) -> no_return().

cl_error(Str) ->
  Msg = lists:flatten(Str),
  throw({dialyzer_cl_parse_error, Msg}).

init() ->
  put(dialyzer_options_mode, cl),
  put(dialyzer_options_files_rec, []),
  put(dialyzer_options_report_mode, normal),
  put(dialyzer_warnings, []),
  DefaultOpts = #options{},
  put(dialyzer_include,           DefaultOpts#options.include_dirs),
  put(dialyzer_options_defines,   DefaultOpts#options.defines),
  put(dialyzer_options_files,     DefaultOpts#options.files),
  put(dialyzer_output_format,     formatted),
  put(dialyzer_filename_opt,      basename),
  put(dialyzer_indent_opt,        ?INDENT_OPT),
  put(dialyzer_options_check_plt, DefaultOpts#options.check_plt),
  put(dialyzer_timing,            DefaultOpts#options.timing),
  put(dialyzer_solvers,           DefaultOpts#options.solvers),
  ok.

append_defines([Def, Val]) ->
  {ok, Tokens, _} = erl_scan:string(Val++"."),
  {ok, ErlVal} = erl_parse:parse_term(Tokens),
  append_var(dialyzer_options_defines, [{list_to_atom(Def), ErlVal}]);
append_defines([Def]) ->
  append_var(dialyzer_options_defines, [{list_to_atom(Def), true}]).

append_include(Dir) ->
  append_var(dialyzer_include, [Dir]).

append_var(Var, List) when is_list(List) ->
  put(Var, get(Var) ++ List),
  ok.

%%-----------------------------------------------------------------------

-spec collect_args([string()]) -> {[string()], [string()]}.

collect_args(List) ->
  collect_args_1(List, []).

collect_args_1(["-"++_|_] = L, Acc) ->
  {lists:reverse(Acc), L};
collect_args_1([Arg|T], Acc) ->
  collect_args_1(T, [Arg|Acc]);
collect_args_1([], Acc) ->
  {lists:reverse(Acc), []}.

%%-----------------------------------------------------------------------

cl_options() ->
  [{files, get(dialyzer_options_files)},
   {files_rec, get(dialyzer_options_files_rec)},
   {output_file, get(dialyzer_output)},
   {output_format, get(dialyzer_output_format)},
   {filename_opt, get(dialyzer_filename_opt)},
   {indent_opt, get(dialyzer_indent_opt)},
   {analysis_type, get(dialyzer_options_analysis_type)},
   {get_warnings, get(dialyzer_options_get_warnings)},
   {timing, get(dialyzer_timing)},
   {callgraph_file, get(dialyzer_callgraph_file)}
   |common_options()].

common_options() ->
  [{defines, get(dialyzer_options_defines)},
   {from, get(dialyzer_options_from)},
   {include_dirs, get(dialyzer_include)},
   {plts, get(dialyzer_init_plts)},
   {output_plt, get(dialyzer_output_plt)},
   {report_mode, get(dialyzer_options_report_mode)},
   {use_spec, get(dialyzer_options_use_contracts)},
   {warnings, get(dialyzer_warnings)},
   {check_plt, get(dialyzer_options_check_plt)},
   {solvers, get(dialyzer_solvers)},
   {native, get(dialyzer_options_native)},
   {native_cache, get(dialyzer_options_native_cache)}].

%%-----------------------------------------------------------------------

-spec get_lib_dir([string()]) -> [string()].

get_lib_dir(Apps) ->
  get_lib_dir(Apps, []).

get_lib_dir([H|T], Acc) ->
  NewElem =
    case code:lib_dir(list_to_atom(H)) of
      {error, bad_name} ->
	case H =:= "erts" of % hack for including erts in an un-installed system
	  true -> filename:join(code:root_dir(), "erts/preloaded/ebin");
	  false -> H
	end;
      LibDir -> LibDir ++ "/ebin"
    end,
  get_lib_dir(T, [NewElem|Acc]);
get_lib_dir([], Acc) ->
  lists:reverse(Acc).

%%-----------------------------------------------------------------------

get_plts(["--"|T], Acc) -> {lists:reverse(Acc), T};
get_plts(["-"++_Opt = H|T], Acc) -> {lists:reverse(Acc), [H|T]};
get_plts([H|T], Acc) -> get_plts(T, [H|Acc]);
get_plts([], Acc) -> {lists:reverse(Acc), []}.

%%-----------------------------------------------------------------------

-spec help_warnings() -> no_return().

help_warnings() ->
  S = warning_options_msg(),
  io:put_chars(S),
  erlang:halt(?RET_NOTHING_SUSPICIOUS).

-spec help_message() -> no_return().

help_message() ->
  S = "Usage: dialyzer [--help] [--version] [--shell] [--quiet] [--verbose]
		[-pa dir]* [--plt plt] [--plts plt*] [-Ddefine]*
                [-I include_dir]* [--output_plt file] [-Wwarn]* [--raw]
                [--src] [--gui] [files_or_dirs] [-r dirs]
                [--apps applications] [-o outfile]
		[--build_plt] [--add_to_plt] [--remove_from_plt]
		[--check_plt] [--no_check_plt] [--plt_info] [--get_warnings]
                [--dump_callgraph file] [--no_native] [--fullpath]
                [--no_indentation] [--statistics] [--no_native_cache]
Options:
  files_or_dirs (for backwards compatibility also as: -c files_or_dirs)
      Use Dialyzer from the command line to detect defects in the
      specified files or directories containing .erl or .beam files,
      depending on the type of the analysis.
  -r dirs
      Same as the previous but the specified directories are searched
      recursively for subdirectories containing .erl or .beam files in
      them, depending on the type of analysis.
  --apps applications
      Option typically used when building or modifying a plt as in:
        dialyzer --build_plt --apps erts kernel stdlib mnesia ...
      to conveniently refer to library applications corresponding to the
      Erlang/OTP installation. However, the option is general and can also
      be used during analysis in order to refer to Erlang/OTP applications.
      In addition, file or directory names can also be included, as in:
        dialyzer --apps inets ssl ./ebin ../other_lib/ebin/my_module.beam
  -o outfile (or --output outfile)
      When using Dialyzer from the command line, send the analysis
      results to the specified outfile rather than to stdout.
  --raw
      When using Dialyzer from the command line, output the raw analysis
      results (Erlang terms) instead of the formatted result.
      The raw format is easier to post-process (for instance, to filter
      warnings or to output HTML pages).
  --src
      Override the default, which is to analyze BEAM files, and
      analyze starting from Erlang source code instead.
  -Dname (or -Dname=value)
      When analyzing from source, pass the define to Dialyzer. (**)
  -I include_dir
      When analyzing from source, pass the include_dir to Dialyzer. (**)
  -pa dir
      Include dir in the path for Erlang (useful when analyzing files
      that have '-include_lib()' directives).
  --output_plt file
      Store the plt at the specified file after building it.
  --plt plt
      Use the specified plt as the initial plt (if the plt was built 
      during setup the files will be checked for consistency).
  --plts plt*
      Merge the specified plts to create the initial plt -- requires
      that the plts are disjoint (i.e., do not have any module
      appearing in more than one plt).
      The plts are created in the usual way:
        dialyzer --build_plt --output_plt plt_1 files_to_include
        ...
        dialyzer --build_plt --output_plt plt_n files_to_include
      and then can be used in either of the following ways:
        dialyzer files_to_analyze --plts plt_1 ... plt_n
      or:
        dialyzer --plts plt_1 ... plt_n -- files_to_analyze
      (Note the -- delimiter in the second case)
  -Wwarn
      A family of options which selectively turn on/off warnings
      (for help on the names of warnings use dialyzer -Whelp).
  --shell
      Do not disable the Erlang shell while running the GUI.
  --version (or -v)
      Print the Dialyzer version and some more information and exit.
  --help (or -h)
      Print this message and exit.
  --quiet (or -q)
      Make Dialyzer a bit more quiet.
  --verbose
      Make Dialyzer a bit more verbose.
  --statistics
      Prints information about the progress of execution (analysis phases,
      time spent in each and size of the relative input).
  --build_plt
      The analysis starts from an empty plt and creates a new one from the
      files specified with -c and -r. Only works for beam files.
      Use --plt(s) or --output_plt to override the default plt location.
  --add_to_plt
      The plt is extended to also include the files specified with -c and -r.
      Use --plt(s) to specify which plt to start from, and --output_plt to
      specify where to put the plt. Note that the analysis might include
      files from the plt if they depend on the new files.
      This option only works with beam files.
  --remove_from_plt
      The information from the files specified with -c and -r is removed
      from the plt. Note that this may cause a re-analysis of the remaining
      dependent files.
  --check_plt
      Check the plt for consistency and rebuild it if it is not up-to-date.
      Actually, this option is of rare use as it is on by default.
  --no_check_plt (or -n)
      Skip the plt check when running Dialyzer. Useful when working with
      installed plts that never change.
  --plt_info
      Make Dialyzer print information about the plt and then quit. The plt
      can be specified with --plt(s).
  --get_warnings
      Make Dialyzer emit warnings even when manipulating the plt. Warnings
      are only emitted for files that are actually analyzed.
  --dump_callgraph file
      Dump the call graph into the specified file whose format is determined
      by the file name extension. Supported extensions are: raw, dot, and ps.
      If something else is used as file name extension, default format '.raw'
      will be used.
  --no_native (or -nn)
      Bypass the native code compilation of some key files that Dialyzer
      heuristically performs when dialyzing many files; this avoids the
      compilation time but it may result in (much) longer analysis time.
  --no_native_cache
      By default, Dialyzer caches the results of native compilation in the
      $XDG_CACHE_HOME/erlang/dialyzer_hipe_cache directory.
      XDG_CACHE_HOME defaults to $HOME/.cache.  Use this option to disable
      caching.
  --fullpath
      Display the full path names of files for which warnings are emitted.
  --no_indentation
      Do not indent contracts and success typings. Note that this option has
      no effect when combined with the --raw option.
  --gui
      Use the GUI.

Note:
  * denotes that multiple occurrences of these options are possible.
 ** options -D and -I work both from command-line and in the Dialyzer GUI;
    the syntax of defines and includes is the same as that used by \"erlc\".

" ++ warning_options_msg() ++ "
The exit status of the command line version is:
  0 - No problems were encountered during the analysis and no
      warnings were emitted.
  1 - Problems were encountered during the analysis.
  2 - No problems were encountered, but warnings were emitted.
",
  io:put_chars(S),
  erlang:halt(?RET_NOTHING_SUSPICIOUS).

warning_options_msg() ->
  "Warning options:
  -Wno_return
     Suppress warnings for functions that will never return a value.
  -Wno_unused
     Suppress warnings for unused functions.
  -Wno_improper_lists
     Suppress warnings for construction of improper lists.
  -Wno_fun_app
     Suppress warnings for fun applications that will fail.
  -Wno_match
     Suppress warnings for patterns that are unused or cannot match.
  -Wno_opaque
     Suppress warnings for violations of opacity of data types.
  -Wno_fail_call
     Suppress warnings for failing calls.
  -Wno_contracts
     Suppress warnings about invalid contracts.
  -Wno_behaviours
     Suppress warnings about behaviour callbacks which drift from the published
     recommended interfaces.
  -Wno_missing_calls
     Suppress warnings about calls to missing functions.
  -Wno_undefined_callbacks
     Suppress warnings about behaviours that have no -callback attributes for
     their callbacks.
  -Wunmatched_returns ***
     Include warnings for function calls which ignore a structured return
     value or do not match against one of many possible return value(s).
  -Werror_handling ***
     Include warnings for functions that only return by means of an exception.
  -Wrace_conditions ***
     Include warnings for possible race conditions.
  -Wunderspecs ***
     Warn about underspecified functions
     (those whose -spec is strictly more allowing than the success typing).
  -Wunknown ***
     Let warnings about unknown functions and types affect the
     exit status of the command line version. The default is to ignore
     warnings about unknown functions and types when setting the exit
     status. When using the Dialyzer from Erlang, warnings about unknown
     functions and types are returned; the default is not to return
     such warnings.

The following options are also available but their use is not recommended:
(they are mostly for Dialyzer developers and internal debugging)
  -Woverspecs ***
     Warn about overspecified functions
     (those whose -spec is strictly less allowing than the success typing).
  -Wspecdiffs ***
     Warn when the -spec is different than the success typing.

*** Identifies options that turn on warnings rather than turning them off.
".
