%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File        : dialyzer.erl
%%% Authors     : Tobias Lindahl <tobiasl@it.uu.se>
%%%               Kostis Sagonas <kostis@it.uu.se>
%%% Description : This is the interface for the Dialyzer tool.
%%%
%%% Created     : 27 Apr 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer).
-moduledoc """
Dialyzer is a **DI**screpancy **A**na**LYZ**er for **ER**lang programs.

Dialyzer is a static analysis tool that identifies software
discrepancies, such as definite type errors, code that is unreachable
because of programming errors, and unnecessary tests in single Erlang
modules or an entire codebase.

Dialyzer starts its analysis from either debug-compiled BEAM code or
from Erlang source code. The file and line number of a discrepancy is
reported along with an indication of the nature of the discrepancy.
Dialyzer bases its analysis on the concept of success typings,
ensuring sound warnings without false positives.

[](){: #command_line }

## Using Dialyzer from the Command Line

This section provides a brief description of the options available
when running Dialyzer from the command line. The same information can
be obtained by writing the following in a shell:

```text
dialyzer --help
```

_Exit status of the command-line version:_

- **`0`** - No problems were found during the analysis and no warnings were
  emitted.

- **`1`** - Problems were found during the analysis.

- **`2`** - No problems were found during the analysis, but warnings were
  emitted.

_Usage:_

```text
dialyzer [--add_to_plt] [--apps applications] [--build_plt]
         [--check_plt] [-Ddefine]* [-Dname]* [--dump_callgraph file]
         [--error_location flag] [files_or_dirs] [--fullpath]
         [--get_warnings] [--help] [-I include_dir]*
         [--incremental] [--metrics_file] [--no_check_plt] [--no_indentation]
         [--no_spec] [-o outfile] [--output_plt file] [-pa dir]* [--plt plt]
         [--plt_info] [--plts plt*] [--quiet] [-r dirs] [--raw]
         [--remove_from_plt] [--shell] [--src] [--statistics] [--verbose]
         [--version] [--warning_apps applications] [-Wwarn]*
```

> #### Note {: .info }
>
> \* denotes that multiple occurrences of the option are possible.

_Options of the command-line version:_

- **`--add_to_plt`** - The PLT is extended to also include the files specified
  with `-c` and `-r`. Use `--plt` to specify which PLT to start from, and
  `--output_plt` to specify where to put the PLT. Note that files already
  included in the PLT will be reanalyzed if they depend on the new files.
  This option only works for BEAM files, not source files.

- **`--apps applications`** - By default, warnings will be reported to all
  applications given by `--apps`. However, if `--warning_apps` is used, only
  those applications given to `--warning_apps` will have warnings reported. All
  applications given by `--apps`, but not `--warning_apps`, will be analysed to
  provide context to the analysis, but warnings will not be reported for them.
  For example, you may want to include libraries you depend on in the analysis
  with `--apps` so discrepancies in their usage can be found, but only include
  your own code with `--warning_apps` so that discrepancies are only reported in
  code that you own.

- **`--warning_apps applications`** - This option is typically used when
  building or modifying a PLT as in:

  ```text
  dialyzer --build_plt --apps erts kernel stdlib mnesia ...
  ```

  to refer conveniently to library applications corresponding to the
  Erlang/OTP installation. This option can also be used during
  analysis to refer to Erlang/OTP applications. File or directory
  names can also be included, as in:

  ```text
  dialyzer --apps inets ssl ./ebin ../other_lib/ebin/my_module.beam
  ```

- **`--build_plt`** - The analysis starts from an empty PLT and creates a new
  one from the files specified with `-c` and `-r`. This option only works for
  BEAM files. To override the default PLT location, use `--plt` or
  `--output_plt`.

- **`--check_plt`** - Check the PLT for consistency and rebuild it if it is not
  up-to-date.

- **`-Dname` (or `-Dname=value`)** - When analyzing from source, pass the define
  to Dialyzer. (\*\*)

- **`--dump_callgraph file`** - Dump the call graph into the specified file
  whose format is determined by the filename extension. Supported extensions
  are: `raw`, `dot`, and `ps`. If something else is used as filename extension,
  the default `.raw` format is used.

- **`--error_location column | line`{: #error_location }** - Use a pair
  `{Line, Column}` or an integer `Line` to pinpoint the location of warnings.
  The default is to use a pair `{Line, Column}`. When formatted, the line and
  the column are separated by a colon.

- **`files_or_dirs` (for backward compatibility also as `-c files_or_dirs`)** -
  Use Dialyzer from the command line to detect defects in the specified files or
  directories containing `.erl` or `.beam` files, depending on the type of the
  analysis.

- **`--fullpath`** - Display the full path names of files for which warnings are
  emitted.

- **`--get_warnings`** - Make Dialyzer emit warnings even when manipulating the
  PLT. Warnings are only emitted for files that are analyzed.

- **`--help` (or `-h`)** - Print a help message and exit.

- **`-I include_dir`** - When analyzing from source, pass the `include_dir` to
  Dialyzer. (\*\*)

- **`--input_list_file file`** - Analyze the file names that are listed in the
  specified file (one file name per line).

- **`--no_check_plt`** - Skip the PLT check when running Dialyzer. This is
  useful when working with installed PLTs that never change.

- **`--incremental`** - The analysis starts from an existing incremental PLT, or
  builds one from scratch if one does not exist, and runs the minimal amount of
  additional analysis to report all issues in the given set of apps. Notably,
  incremental PLT files are not compatible with "classic" PLT files, and vice
  versa. The initial incremental PLT will be updated unless an alternative
  output incremental PLT is given.

- **`--no_indentation`** - Do not insert line breaks in types, contracts, and
  Erlang Code when formatting warnings.

- **`--no_spec`** - Ignore functions specs. This is useful for debugging when
  one suspects that some specs are incorrect.

- **`-o outfile` (or `--output outfile`)** - When using Dialyzer from the
  command line, send the analysis results to the specified outfile rather than
  to `stdout`.

- **`--metrics_file file`** - Write metrics about Dialyzer's incrementality (for
  example, total number of modules considered, how many modules were changed
  since the PLT was last updated, how many modules needed to be analyzed) to a
  file. This can be useful for tracking and debugging Dialyzer's incrementality.

- **`--output_plt file`** - Store the PLT at the specified file after building
  it.

- **`-pa dir`** - Include `dir` in the path for Erlang. This is useful when
  analyzing files that have `-include_lib()` directives.

- **`--plt plt`** - Use the specified PLT as the initial PLT. If the PLT was
  built during setup, the files are checked for consistency.

- **`--plt_info`** - Make Dialyzer print information about the PLT and then
  quit. The PLT can be specified with `--plt(s)`.

- **`--plts plt*`** - Merge the specified PLTs to create the initial PLT. This
  requires that the PLTs are disjoint (that is, do not have any module appearing
  in more than one PLT). The PLTs are created in the usual way:

  ```text
  dialyzer --build_plt --output_plt plt_1 files_to_include
  ...
  dialyzer --build_plt --output_plt plt_n files_to_include
  ```

  They can then be used in either of the following ways:

  ```text
  dialyzer files_to_analyze --plts plt_1 ... plt_n
  ```

  or

  ```text
  dialyzer --plts plt_1 ... plt_n -- files_to_analyze
  ```

  Notice the `--` delimiter in the second case.

- **`--quiet` (or `-q`)** - Make Dialyzer a bit more quiet.

- **`-r dirs`** - Same as `files_or_dirs`, but the specified directories are
  searched recursively for subdirectories containing `.erl` or `.beam` files in
  them, depending on the type of analysis.

- **`--raw`** - When using Dialyzer from the command line, output the raw
  analysis results (Erlang terms) instead of the formatted result. The raw
  format is easier to post-process (for example, to filter warnings or to output
  HTML pages).

- **`--remove_from_plt`** - The information from the files specified with `-c`
  and `-r` is removed from the PLT. Notice that this can cause a reanalysis of
  the remaining dependent files.

- **`--src`** - Override the default, which is to analyze BEAM files, and
  analyze starting from Erlang source code instead.

- **`--statistics`** - Print information about the progress of execution
  (analysis phases, time spent in each, and size of the relative input).

- **`--verbose`** - Make Dialyzer a bit more verbose.

- **`--version` (or `-v`)** - Print the Dialyzer version and some more
  information and exit.

- **`-Wwarn`** - A family of options that selectively turn on/off warnings. (For
  help on the names of warnings, use `dialyzer -Whelp`.) Notice that the options
  can also be specified in the file with a `-dialyzer()` attribute. For details,
  see section
  [Requesting or Suppressing Warnings in Source Files](`m:dialyzer#suppression`).

> #### Note {: .info }
>
> \*\* the syntax of defines and includes is the same as that used by
> [erlc](`e:erts:erlc_cmd.md`).

[](){: #warning_options }

_Warning options:_

- **`-Werror_handling` (\*\*\*)** - Include warnings for functions that only
  return by an exception.

- **`-Wextra_return` (\*\*\*)** - Warn about functions whose specification
  includes types that the function cannot return.

- **`-Wmissing_return` (\*\*\*)** - Warn about functions that return values that
  are not part of the specification.

- **`-Wno_behaviours`** - Suppress warnings about behavior callbacks that drift
  from the published recommended interfaces.

- **`-Wno_contracts`** - Suppress warnings about invalid contracts.

- **`-Wno_fail_call`** - Suppress warnings for failing calls.

- **`-Wno_fun_app`** - Suppress warnings for fun applications that will fail.

- **`-Wno_improper_lists`** - Suppress warnings for construction of improper
  lists.

- **`-Wno_match`** - Suppress warnings for patterns that are unused or cannot
  match.

- **`-Wno_missing_calls`** - Suppress warnings about calls to missing functions.

- **`-Wno_opaque`** - Suppress warnings for violations of opacity of data types.

- **`-Wno_return`** - Suppress warnings for functions that will never return a
  value.

- **`-Wno_undefined_callbacks`** - Suppress warnings about behaviors that have
  no `-callback` attributes for their callbacks.

- **`-Wno_unused`** - Suppress warnings for unused functions.

- **`-Wno_unknown`** - Suppress warnings about unknown functions and types. The
  default is to warn about unknown functions and types when setting the exit
  status. When using Dialyzer from Erlang, warnings about unknown functions and
  types are returned.

- **`-Wunderspecs` (\*\*\*)** - Warn about underspecified functions (the
  specification is strictly more allowing than the success typing).

- **`-Wunmatched_returns` (\*\*\*)** - Include warnings for function calls that
  ignore a structured return value or do not match against one of many possible
  return values. However, no warnings are included if the possible return values
  are a union of atoms or a union of numbers.

The following options are also available, but their use is not recommended (they
are mostly for Dialyzer developers and internal debugging):

- **`-Woverspecs` (\*\*\*)** - Warn about overspecified functions (the
  specification is strictly less allowing than the success typing).

- **`-Wspecdiffs` (\*\*\*)** - Warn when the specification is different than the
  success typing.

> #### Note {: .info }
>
> \*\*\* denotes options that turn on warnings rather than turning them off.

The following options are not strictly needed as they specify the
default. They are primarily intended to be used with the `-dialyzer`
attribute. For an example see section [Requesting or Suppressing
Warnings in Source Files](`m:dialyzer#suppression`).

- **`-Wno_underspecs`** - Suppress warnings about underspecified functions (the
  specification is strictly more allowing than the success typing).

- **`-Wno_extra_return`** - Suppress warnings about functions whose
  specification includes types that the function cannot return.

- **`-Wno_missing_return`** - Suppress warnings about functions that return
  values that are not part of the specification.

## Using Dialyzer from Erlang

Dialyzer can be used directly from Erlang. The options are similar to the ones
given from the command line. See section
[Using Dialyzer from the Command Line](`m:dialyzer#command_line`).

## Default Dialyzer Options

The (host operating system) environment variable `ERL_COMPILER_OPTIONS` can be
used to give default Dialyzer options. Its value must be a valid Erlang term. If
the value is a list, it is used as is. If it is not a list, it is put into a
list.

The list is appended to any options given to `run/1` or on the command line.

The list can be retrieved with `compile:env_compiler_options/0`.

Currently the only option used is the
[`error_location`](`m:dialyzer#error_location`) option.

_Dialyzer configuration file:_

Dialyzer's configuration file may also be used to augment the default options
and those given directly to the Dialyzer command. It is commonly used to avoid
repeating options which would otherwise need to be given explicitly to Dialyzer
on every invocation.

The location of the configuration file can be set via the `DIALYZER_CONFIG`
environment variable, and defaults to within the `user_config` from
`filename:basedir/3`.

An example configuration file's contents might be:

```erlang
      {incremental,
        {default_apps,[stdlib,kernel,erts]},
        {default_warning_apps,[stdlib]}
      }.
      {warnings, [no_improper_lists]}.
      {add_pathsa,["/users/samwise/potatoes/ebin"]}.
      {add_pathsz,["/users/smeagol/fish/ebin"]}.
```

[](){: #suppression }

## Requesting or Suppressing Warnings in Source Files

Attribute `-dialyzer()` can be used for turning off warnings in a module by
specifying functions or warning options. For example, to turn off all warnings
for the function `f/0`, include the following line:

```erlang
-dialyzer({nowarn_function, f/0}).
```

To turn off warnings for improper lists, add the following line to the source
file:

```text
-dialyzer(no_improper_lists).
```

Attribute `-dialyzer()` is allowed after function declarations. Lists of warning
options or functions are allowed:

```erlang
-dialyzer([{nowarn_function, [f/0]}, no_improper_lists]).
```

Warning options can be restricted to functions:

```erlang
-dialyzer({no_improper_lists, g/0}).
```

```erlang
-dialyzer({[no_return, no_match], [g/0, h/0]}).
```

The warning option for underspecified functions, `-Wunderspecs`, can result in
useful warnings, but often functions with specifications that are strictly more
allowing than the success typing cannot easily be modified to be less allowing.
To turn off the warning for underspecified function `f/0`, include the following
line:

```erlang
-dialyzer({no_underspecs, f/0}).
```

For help on the warning options, use `dialyzer -Whelp`. The options are also
enumerated, see type `t:warn_option/0`.

Attribute `-dialyzer()` can also be used for turning on warnings. For example,
if a module has been fixed regarding unmatched returns, adding the following
line can help in assuring that no new unmatched return warnings are introduced:

```text
-dialyzer(unmatched_returns).
```
""".

%%--------------------------------------------------------------------
%% NOTE: Only functions exported by this module are available to
%%       other applications.
%%--------------------------------------------------------------------
-export([plain_cl/0,
	 run/1,
	 run_report_modules_analyzed/1,
	 run_report_modules_changed_and_analyzed/1,
	 plt_info/1,
	 format_warning/1,
	 format_warning/2]).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------
%% Interfaces:
%%  - plain_cl/0 :      to be used ONLY by the dialyzer C program.
%%  - run/1:            Erlang interface for a command line-like analysis
%%  - run_report_modules_analyzed/1: Erlang interface for a command line-like
%%                      analysis, but also returns the list of modules that
%%                      had to be analyzed to compute the result
%%  - run_report_modules_analyzed/1: Erlang interface for a command line-like
%%                      analysis, but also returns the list of modules that
%%                      had to be analyzed to compute the result, plus the
%%                      set of modules that have changed since the PLT was
%%                      created (if applicable)
%%  - format_warning/1: Get the string representation of a warning.
%%  - format_warning/2: Likewise, but with an option whether
%%			to display full path names or not
%%  - plt_info/1:       Get information of the specified plt.
%%--------------------------------------------------------------------

-doc false.
-spec plain_cl() -> no_return().

plain_cl() ->
  case dialyzer_cl_parse:start() of
    {check_init, Opts} ->
      cl_halt(cl_check_init(Opts), Opts);
    {plt_info, Opts} ->
      cl_halt(cl_print_plt_info(Opts), Opts);
    {cl, Opts} ->
      case Opts#options.check_plt of
	true ->
	  case cl_check_init(Opts#options{get_warnings = false}) of
	    {error, _} = Error -> cl_halt(Error, Opts);
	    {ok, _} -> cl_halt(cl(Opts), Opts)
	  end;
	false ->
	  cl_halt(cl(Opts), Opts)
      end;
    {error, Msg} ->
      cl_error(Msg)
  end.

cl_check_init(#options{analysis_type = AnalType} = Opts) ->
  case AnalType of
    plt_build ->  {ok, ?RET_NOTHING_SUSPICIOUS};
    plt_add ->    {ok, ?RET_NOTHING_SUSPICIOUS};
    plt_remove -> {ok, ?RET_NOTHING_SUSPICIOUS};
    incremental -> {ok, ?RET_NOTHING_SUSPICIOUS};
    Other when Other =:= succ_typings; Other =:= plt_check ->
      F = fun() ->
	      NewOpts = Opts#options{analysis_type = plt_check},
	      {Ret, _Warnings} = dialyzer_cl:start(NewOpts),
	      Ret
	  end,
      doit(F)
  end.

cl_print_plt_info(Opts) ->
  F = fun() ->
	  print_plt_info(Opts)
      end,
  doit(F).

print_plt_info(#options{init_plts = PLTs, output_file = OutputFile}) ->
  PLTInfo = get_plt_info(PLTs),
  do_print_plt_info(PLTInfo, OutputFile).

get_plt_info([PLT|PLTs]) ->
  String =
    case dialyzer_plt:plt_kind(PLT) of
      cplt ->
        case dialyzer_cplt:included_files(PLT) of
          {ok, Files} ->
            io_lib:format("The classic PLT ~ts includes the following files:\n~tp\n\n",
                    [PLT, Files]);
          {error, read_error} ->
            Msg = io_lib:format("Could not read the classic PLT file ~tp\n\n", [PLT]),
            throw({dialyzer_error, Msg});
          {error, no_such_file} ->
            Msg = io_lib:format("The classic PLT file ~tp does not exist\n\n", [PLT]),
            throw({dialyzer_error, Msg})
        end;
      iplt ->
        case dialyzer_iplt:included_modules(PLT) of
          {ok, Modules} ->
            io_lib:format("The incremental PLT ~ts includes the following modules:\n~tp\n\n",
                    [PLT, Modules]);
          {error, read_error} ->
            Msg = io_lib:format("Could not read the incremental PLT file ~tp\n\n", [PLT]),
            throw({dialyzer_error, Msg});
          {error, no_such_file} ->
            Msg = io_lib:format("The incremental PLT file ~tp does not exist\n\n", [PLT]),
            throw({dialyzer_error, Msg})
        end;
      bad_file ->
        Msg = io_lib:format("Could not read the PLT file ~tp\n\n", [PLT]),
        throw({dialyzer_error, Msg});
      no_file ->
        Msg = io_lib:format("The PLT file ~tp does not exist\n\n", [PLT]),
        throw({dialyzer_error, Msg})
    end,
  String ++ get_plt_info(PLTs);
get_plt_info([]) -> "".

do_print_plt_info(PLTInfo, OutputFile) ->
  case OutputFile =:= none of
    true ->
      io:format("~ts", [PLTInfo]),
      ?RET_NOTHING_SUSPICIOUS;
    false ->
      case file:open(OutputFile, [write]) of
	{ok, FileDesc} ->
	  io:format(FileDesc, "~ts", [PLTInfo]),
	  ok = file:close(FileDesc),
	  ?RET_NOTHING_SUSPICIOUS;
	{error, Reason} ->
	  Msg1 = io_lib:format("Could not open output file ~tp, Reason: ~p\n",
			       [OutputFile, Reason]),
	  throw({dialyzer_error, Msg1})
      end
  end.

cl(Opts) ->
  F =
    fun() ->
        {Ret, _Warnings} =
          case Opts#options.analysis_type of
            incremental ->
              dialyzer_incremental:start(Opts);
            _ ->
              dialyzer_cl:start(Opts)
          end,
        Ret
    end,
  doit(F).

-doc "Run Dialyzer and return warnings.".
-spec run(Options) -> Warnings when
    Options :: [dial_option()],
    Warnings :: [dial_warning()].

run(Opts) ->
  {Warnings, _ModulesAnalyzed} = run_report_modules_analyzed(Opts),
  Warnings.

-doc false.
-spec run_report_modules_analyzed(Options) -> {Warnings, ModulesAnalyzed} when
    Options :: [dial_option()],
    Warnings :: [dial_warning()],
    ModulesAnalyzed :: [module()].

run_report_modules_analyzed(Opts) ->
  {Warnings, _ModulesChanged, ModulesAnalyzed} = run_report_modules_changed_and_analyzed(Opts),
  {Warnings, ModulesAnalyzed}.

-doc false.
-spec run_report_modules_changed_and_analyzed(Options) -> {Warnings, ModulesChanged, ModulesAnalyzed} when
    Options :: [dial_option()],
    Warnings :: [dial_warning()],
    ModulesChanged :: undefined | [module()],
    ModulesAnalyzed :: [module()].

run_report_modules_changed_and_analyzed(Opts) ->
  try dialyzer_options:build([{report_mode, quiet},
			      {erlang_mode, true}|Opts]) of
    {error, Msg} ->
      throw({dialyzer_error, Msg});
    OptsRecord ->
      ok = check_init(OptsRecord),
      AnalysisResult =
        case OptsRecord#options.analysis_type of
          incremental ->
            dialyzer_incremental:start_report_modules_changed_and_analyzed(OptsRecord);
          _ ->
            dialyzer_cl:start_report_modules_changed_and_analyzed(OptsRecord)
        end,
      case AnalysisResult of
        {{?RET_DISCREPANCIES, Warnings}, ModulesChanged, ModulesAnalyzed} -> {Warnings, ModulesChanged, ModulesAnalyzed};
        {{?RET_NOTHING_SUSPICIOUS, _}, ModulesChanged, ModulesAnalyzed}  -> {[], ModulesChanged, ModulesAnalyzed}
      end
  catch
    throw:{dialyzer_error, ErrorMsg} ->
      erlang:error({dialyzer_error, lists:flatten(ErrorMsg)})
  end.

check_init(#options{analysis_type = plt_check}) ->
    ok;
check_init(#options{check_plt = true} = OptsRecord) ->
    case cl_check_init(OptsRecord) of
	{ok, _} -> ok;
	{error, Msg} -> throw({dialyzer_error, Msg})
    end;
check_init(#options{check_plt = false}) ->
    ok.

-doc "Returns information about the specified PLT.".
-spec plt_info(Plt) ->
     {'ok', ClassicResult | IncrementalResult } | {'error', Reason} when
    Plt :: file:filename(),
    ClassicResult :: [{'files', [file:filename()]}],
    IncrementalResult :: {incremental, [{'modules', [module()]}]},
    Reason :: 'not_valid' | 'no_such_file' | 'read_error'.

plt_info(Plt) ->
  case dialyzer_plt:plt_kind(Plt) of
    cplt ->
      case dialyzer_cplt:included_files(Plt) of
        {ok, Files} -> {ok, [{files, Files}]};
        Error -> Error
      end;
    iplt ->
      case dialyzer_iplt:included_modules(Plt) of
        {ok, Modules} -> {ok, {incremental, [{modules, Modules}]}};
        Error -> Error
      end;
    bad_file -> {error, not_valid};
    no_file -> {error, no_such_file}
  end.


%%-----------
%% Machinery
%%-----------

-type doit_ret() :: {'ok', dial_ret()} | {'error', string()}.

doit(F) ->
  try
    {ok, F()}
  catch
    throw:{dialyzer_error, Msg} ->
      {error, lists:flatten(Msg)}
  end.

-spec cl_error(string()) -> no_return().

cl_error(Msg) ->
  cl_halt({error, Msg}, #options{}).

-spec cl_halt(doit_ret(), #options{}) -> no_return().

cl_halt({ok, R = ?RET_NOTHING_SUSPICIOUS}, #options{report_mode = quiet}) ->
  halt(R);
cl_halt({ok, R = ?RET_DISCREPANCIES}, #options{report_mode = quiet}) ->
  halt(R);
cl_halt({ok, R = ?RET_NOTHING_SUSPICIOUS}, #options{}) ->
  io:put_chars("done (passed successfully)\n"),
  halt(R);
cl_halt({ok, R = ?RET_DISCREPANCIES}, #options{output_file = Output}) ->
  io:put_chars("done (warnings were emitted)\n"),
  cl_check_log(Output),
  halt(R);
cl_halt({error, Msg1}, #options{output_file = Output}) ->
  %% Msg2 = "dialyzer: Internal problems were encountered in the analysis",
  io:format("\ndialyzer: ~ts\n", [Msg1]),
  cl_check_log(Output),
  halt(?RET_INTERNAL_ERROR).

-spec cl_check_log('none' | file:filename()) -> 'ok'.

cl_check_log(none) ->
  ok;
cl_check_log(Output) ->
  io:format("  Check output file `~ts' for details\n", [Output]).

-doc "Get a string from warnings as returned by `run/1`.".
-spec format_warning(Warnings) -> string() when
    %% raw_warning() | % not documented
    Warnings :: dial_warning().

format_warning(W) ->
  format_warning(W, basename).

-type format_option()  :: {'indent_opt', boolean()}
                        | {'filename_opt', filename_opt()}
                        | {'error_location', error_location()}.

-doc """
Get a string from warnings as returned by `run/1`.

If `indent_opt` is set to `true` (default), line breaks are inserted in types,
contracts, and Erlang code to improve readability.

If `error_location` is set to `column` (default), locations are formatted as
`Line:Column` if the column number is available, otherwise locations are
formatted as `Line` even if the column number is available.
""".
-doc(#{since => <<"R14B02">>}).
-spec format_warning(Warnings, Options) -> string() when
    %% raw_warning() | % not documented
    Warnings :: dial_warning(),
    Options :: filename_opt() | [format_option()].

format_warning(RawWarning, FOpt) when is_atom(FOpt) ->
  format_warning(RawWarning, [{filename_opt, FOpt}]);
format_warning({Tag, {File, Location, _MFA}, Msg}, Opts) ->
  format_warning({Tag, {File, Location}, Msg}, Opts);
format_warning({_Tag, {File, Location}, Msg}, Opts) when is_list(File) ->
  F = case proplists:get_value(filename_opt, Opts, basename) of
	fullpath -> File;
	basename -> filename:basename(File)
      end,
  Indent = proplists:get_value(indent_opt, Opts, ?INDENT_OPT),
  ErrorLocation =
    proplists:get_value(error_location, Opts, ?ERROR_LOCATION),
  String = message_to_string(Msg, Indent, ErrorLocation),
  PosString = pos(Location, ErrorLocation),
  lists:flatten(io_lib:format("~ts:~s: ~ts", [F, PosString, String])).

pos({Line, _Column}, line) ->
  pos(Line);
pos(Location, _ErrorLocation) ->
  pos(Location).

pos({Line, Column}) when is_integer(Line), is_integer(Column) ->
    io_lib:format("~w:~w", [Line, Column]);
pos(Line) when is_integer(Line) ->
    io_lib:format("~w", [Line]).

%%-----------------------------------------------------------------------------
%% Message classification and pretty-printing below. Messages appear in
%% categories and in more or less alphabetical ordering within each category.
%%-----------------------------------------------------------------------------

%%----- Warnings for general discrepancies ----------------
message_to_string({apply, [Args, ArgNs, FailReason,
			   SigArgs, SigRet, Contract]}, I, _E) ->
  io_lib:format("Fun application with arguments ~ts ", [a(Args, I)]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract, I);
message_to_string({app_call, [M, F, Args, Culprit, ExpectedType, FoundType]},
                  I, _E) ->
  io_lib:format("The call ~s:~ts~ts requires that ~ts is of type ~ts not ~ts\n",
		[M, F, a(Args, I), c(Culprit, I),
                 t(ExpectedType, I), t(FoundType, I)]);
message_to_string({bin_construction, [Culprit, Size, Seg, Type]}, I, _E) ->
  io_lib:format("Binary construction will fail since the ~ts field ~ts in"
		" segment ~ts has type ~ts\n",
                [Culprit, c(Size, I), c(Seg, I), t(Type, I)]);
message_to_string({call, [M, F, Args, ArgNs, FailReason,
			  SigArgs, SigRet, Contract]}, I, _E) ->
  io_lib:format("The call ~w:~tw~ts ", [M, F, a(Args, I)]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract, I);
message_to_string({call_to_missing, [M, F, A]}, _I, _E) ->
  io_lib:format("Call to missing or unexported function ~w:~tw/~w\n",
                [M, F, A]);
message_to_string({exact_compare, [Type1, Op, Type2]}, I, _E) ->
  io_lib:format("The test ~ts ~s ~ts can never evaluate to '~w'\n",
                [t(Type1, I), Op, t(Type2, I),
                 (Op =:= '=:=' orelse Op =:= '==')]);
message_to_string({fun_app_args, [ArgNs, Args, Type]}, I, _E) ->
  PositionString = form_position_string(ArgNs),
  io_lib:format("Fun application with arguments ~ts will fail"
		" since the function has type ~ts,"
                " which differs in the ~s argument\n",
                [a(Args, I), t(Type, I), PositionString]);
message_to_string({fun_app_no_fun, [Op, Type, Arity]}, I, _E) ->
  io_lib:format("Fun application will fail since ~ts :: ~ts"
		" is not a function of arity ~w\n", [Op, t(Type, I), Arity]);
message_to_string({guard_fail, []}, _I, _E) ->
  "Clause guard cannot succeed.\n";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}, I, _E) ->
  io_lib:format("Guard test ~ts ~s ~ts can never succeed\n",
                [a(Arg1, I), Infix, a(Arg2, I)]); % a/2 rather than c/2
message_to_string({map_update, [Type, Key]}, I, _E) ->
  io_lib:format("A key of type ~ts cannot exist "
		"in a map of type ~ts\n", [t(Key, I), t(Type, I)]);
message_to_string({neg_guard_fail, [Arg1, Infix, Arg2]}, I, _E) ->
  io_lib:format("Guard test not(~ts ~s ~ts) can never succeed\n",
		[a(Arg1, I), Infix, a(Arg2, I)]); % a/2 rather than c/2
message_to_string({guard_fail, [Guard, Args]}, I, _E) ->
  io_lib:format("Guard test ~s~ts can never succeed\n", [Guard, a(Args, I)]);
message_to_string({neg_guard_fail, [Guard, Args]}, I, _E) ->
  io_lib:format("Guard test not(~s~ts) can never succeed\n",
                [Guard, a(Args, I)]);
message_to_string({guard_fail_pat, [Pat, Type]}, I, _E) ->
  io_lib:format("Clause guard cannot succeed. The ~ts was matched"
		" against the type ~ts\n", [ps(Pat, I), t(Type, I)]);
message_to_string({improper_list_constr, [TlType]}, I, _E) ->
  io_lib:format("Cons will produce an improper list"
		" since its 2nd argument is ~ts\n", [t(TlType, I)]);
message_to_string({no_return, [Type|Name]}, _I, _E) ->
  NameString =
    case Name of
      [] -> "The created fun ";
      [F, A] -> io_lib:format("Function ~tw/~w ", [F, A])
    end,
  case Type of
    no_match -> NameString ++ "has no clauses that will ever match\n";
    only_explicit -> NameString ++ "only terminates with explicit exception\n";
    only_normal -> NameString ++ "has no local return\n";
    both -> NameString ++ "has no local return\n"
  end;
message_to_string({record_constr, [RecConstr, FieldDiffs]}, I, _E) ->
  io_lib:format("Record construction ~ts violates the"
		" declared type of field ~ts\n",
                [t(RecConstr, I), field_diffs(FieldDiffs, I)]);
message_to_string({record_constr, [Name, Field, Type]}, I, _E) ->
  io_lib:format("Record construction violates the declared type for #~tw{}"
		" since ~ts cannot be of type ~ts\n",
                [Name, ps(Field, I), t(Type, I)]);
message_to_string({record_matching, [String, Name]}, I, _E) ->
  io_lib:format("The ~ts violates the"
		" declared type for #~tw{}\n", [rec_type(String, I), Name]);
message_to_string({record_match, [Pat, Type]}, I, _E) ->
  io_lib:format("Matching of ~ts tagged with a record name violates"
                " the declared type of ~ts\n", [ps(Pat, I), t(Type, I)]);
message_to_string({pattern_match, [Pat, Type]}, I, _E) ->
  io_lib:format("The ~ts can never match the type ~ts\n",
                [ps(Pat, I), t(Type, I)]);
message_to_string({pattern_match_cov, [Pat, Type]}, I, _E) ->
  io_lib:format("The ~ts can never match since previous"
		" clauses completely covered the type ~ts\n",
		[ps(Pat, I), t(Type, I)]);
message_to_string({unmatched_return, [Type]}, I, _E) ->
  io_lib:format("Expression produces a value of type ~ts,"
		" but this value is unmatched\n", [t(Type, I)]);
message_to_string({unused_fun, [F, A]}, _I, _E) ->
  io_lib:format("Function ~tw/~w will never be called\n", [F, A]);
%%----- Warnings for specs and contracts -------------------
message_to_string({contract_diff, [M, F, _A, Contract, Sig]}, I, _E) ->
  io_lib:format("Type specification ~ts"
		" is not equal to the success typing: ~ts\n",
		[con(M, F, Contract, I), con(M, F, Sig, I)]);
message_to_string({contract_subtype, [M, F, _A, Contract, Sig]}, I, _E) ->
  io_lib:format("Type specification ~ts"
		" is a subtype of the success typing: ~ts\n",
		[con(M, F, Contract, I), con(M, F, Sig, I)]);
message_to_string({contract_supertype, [M, F, _A, Contract, Sig]}, I, _E) ->
  io_lib:format("Type specification ~ts"
		" is a supertype of the success typing: ~ts\n",
		[con(M, F, Contract, I), con(M, F, Sig, I)]);
message_to_string({contract_range, [Contract, M, F, ArgStrings,
                                    Location, CRet]}, I, E) ->
  io_lib:format("The contract ~ts cannot be right because the inferred"
		" return for ~tw~ts on position ~s is ~ts\n",
		[con(M, F, Contract, I), F, a(ArgStrings, I),
                 pos(Location, E), t(CRet, I)]);
message_to_string({invalid_contract, [M, F, A, none, Contract, Sig]}, I, _E) ->
  io_lib:format("Invalid type specification for function ~w:~tw/~w.\n"
		" The success typing is ~ts\n"
		" But the spec is ~ts\n", [M, F, A, con(M, F, Sig, I), con(M, F, Contract, I)]);
message_to_string({invalid_contract, [M, F, A, InvalidContractDetails, Contract, Sig]}, I, _E) ->
  io_lib:format("Invalid type specification for function ~w:~tw/~w.\n"
		" The success typing is ~ts\n"
		" But the spec is ~ts\n"
		"~ts",
    [M, F, A, con(M, F, Sig, I), con(M, F, Contract, I),
     format_invalid_contract_details(InvalidContractDetails)]);
message_to_string({contract_with_opaque, [M, F, A, OpaqueType, SigType]},
                 I, _E) ->
  io_lib:format("The specification for ~w:~tw/~w"
                " has an opaque subtype ~ts which is violated by the"
                " success typing ~ts\n",
                [M, F, A, t(OpaqueType, I), sig(SigType, I)]);
message_to_string({extra_range, [M, F, A, ExtraRanges, SigRange]}, I, _E) ->
  io_lib:format("The specification for ~w:~tw/~w states that the function"
		" might also return ~ts but the inferred return is ~ts\n",
		[M, F, A, t(ExtraRanges, I), t(SigRange, I)]);
message_to_string({missing_range, [M, F, A, ExtraRanges, ContrRange]}, I, _E) ->
  io_lib:format("The success typing for ~w:~tw/~w implies that the function"
		" might also return ~ts but the specification return is ~ts\n",
		[M, F, A, t(ExtraRanges, I), t(ContrRange, I)]);
message_to_string({overlapping_contract, [M, F, A]}, _I, _E) ->
  io_lib:format("Overloaded contract for ~w:~tw/~w has overlapping domains;"
		" such contracts cannot establish a dependency between the overloaded input and output types\n",
		[M, F, A]);
message_to_string({spec_missing_fun, [M, F, A]}, _I, _E) ->
  io_lib:format("Contract for function that does not exist: ~w:~tw/~w\n",
		[M, F, A]);
%%----- Warnings for opaque type violations -------------------
message_to_string({call_with_opaque,
                   [M, F, Args, Conflicts, ExpectedTypes]}, I, _E) ->
  Positions = [N || {N, _T, _TStr} <- Conflicts],
  io_lib:format("The call ~w:~tw~ts contains ~ts when ~ts\n",
                [M, F, a(Args, I), form_positions(Positions),
                 form_expected(ExpectedTypes, I)]);
message_to_string({call_without_opaque,
                   [M, F, Args, Conflicts, _ExpectedTypes]}, I, _E) ->
  io_lib:format("The call ~w:~tw~ts does not have ~ts\n",
                [M, F, a(Args, I),
                 form_expected_without_opaque(Conflicts, I)]);
message_to_string({opaque_compare, [Type, Op, OpaqueType]}, I, _E) ->
  Kind = if
            Op =:= '=:='; Op =:= '==' -> "equality";
            Op =:= '=/='; Op =:= '/=' -> "inequality"
         end,
  io_lib:format("Attempt to test for ~ts between a term of type ~ts"
                " and a term of opaque type ~ts\n",
                [Kind, t(Type, I), t(OpaqueType, I)]);
message_to_string({opaque_guard, [Arg1, Infix, Arg2, ArgNs]}, I, _E) ->
  io_lib:format("Guard test ~ts ~s ~ts contains ~s\n",
		[a(Arg1, I), Infix, a(Arg2, I), form_positions(ArgNs)]);
message_to_string({opaque_guard, [Guard, Args]}, I, _E) ->
  io_lib:format("Guard test ~w~ts breaks the opacity of its argument\n",
		[Guard, a(Args, I)]);
message_to_string({opaque_match, [Pat, OpaqueType, OpaqueTerm]}, I, _E) ->
  Term = if OpaqueType =:= OpaqueTerm -> "the term";
            true -> "a term of type " ++ t(OpaqueTerm, I)
         end,
  io_lib:format("The attempt to match ~ts against the "
                "~ts breaks the opacity of the term\n",
                [Term, ps(Pat, I)]);
message_to_string({opaque_union, [IsOpaque, Type]}, I, _E) ->
  TypeString = t(Type, I),
  case IsOpaque of
    true ->
      io_lib:format("Body yields the opaque type ~ts whose opacity is "
                    "broken by the other clauses.\n", [TypeString]);
    false ->
      io_lib:format("Body yields the type ~ts which violates the "
                    "opacity of the other clauses.\n", [TypeString])
  end;
message_to_string({opaque_type_test, [Fun, Args, Arg, ArgType]}, I, _E) ->
  io_lib:format("The type test ~ts~ts breaks the opacity of the term ~ts~ts\n",
                [Fun, a(Args, I), Arg, t(ArgType, I)]);
message_to_string({opaque_size, [SizeType, Size]}, I, _E) ->
  io_lib:format("The size ~ts breaks the opacity of ~ts\n",
                [t(SizeType, I), c(Size, I)]);
message_to_string({opaque_call, [M, F, Args, Culprit, OpaqueType]}, I, _E) ->
  io_lib:format("The call ~s:~ts~ts breaks the opacity of the term ~ts :: ~ts\n",
                [M, F, a(Args, I), c(Culprit, I), t(OpaqueType, I)]);
%%----- Warnings for behaviour errors --------------------
message_to_string({callback_type_mismatch, [B, F, A, ST, CT]}, I, _E) ->
  io_lib:format("The inferred return type of ~tw/~w ~ts has nothing in"
                " common with ~ts, which is the expected return type for"
                " the callback of the ~w behaviour\n",
                [F, A, t("("++ST++")", I), t(CT, I), B]);
message_to_string({callback_arg_type_mismatch, [B, F, A, N, ST, CT]}, I, _E) ->
  io_lib:format("The inferred type for the ~s argument of ~tw/~w (~ts)"
		" has nothing in common with ~ts, which is expected type for this"
		" argument in the callback of the ~w behaviour\n",
		[ordinal(N), F, A, t(ST, I), t(CT, I), B]);
message_to_string({callback_spec_type_mismatch, [B, F, A, ST, CT]}, I, _E) ->
  io_lib:format("The return type ~ts in the specification of ~tw/~w has nothing"
		" in common with ~ts, which is the expected return type for the"
		" callback of the ~w behaviour\n",
                [t(ST, I), F, A, t(CT, I), B]);
message_to_string({callback_spec_arg_type_mismatch, [B, F, A, N, ST, CT]},
                  I, _E) ->
  io_lib:format("The specified type for the ~ts argument of ~tw/~w (~ts) has"
		" nothing in common with ~ts, which is expected type for this"
		" argument in the callback of the ~w behaviour\n",
		[ordinal(N), F, A, t(ST, I), t(CT, I), B]);
message_to_string({callback_missing, [B, F, A]}, _I, _E) ->
  io_lib:format("Undefined callback function ~tw/~w (behaviour ~w)\n",
		[F, A, B]);
message_to_string({callback_not_exported, [B, F, A]}, _I, _E) ->
  io_lib:format("Callback function ~tw/~w exists but is not exported (behaviour ~w)\n",
		[F, A, B]);
message_to_string({callback_info_missing, [B]}, _I, _E) ->
  io_lib:format("Callback info about the ~w behaviour is not available\n", [B]);
%%----- Warnings for unknown functions, types, and behaviours -------------
message_to_string({unknown_type, {M, F, A}}, _I, _E) ->
  io_lib:format("Unknown type ~w:~tw/~w\n", [M, F, A]);
message_to_string({unknown_function, {M, F, A}}, _I, _E) ->
  io_lib:format("Unknown function ~w:~tw/~w\n", [M, F, A]);
message_to_string({unknown_behaviour, B}, _I, _E) ->
  io_lib:format("Unknown behaviour ~w\n", [B]).

%%-----------------------------------------------------------------------------
%% Auxiliary functions below
%%-----------------------------------------------------------------------------

format_invalid_contract_details({InvalidArgIdxs, IsRangeInvalid}) ->
  ArgOrd = form_position_string(InvalidArgIdxs),
  ArgDesc =
    case InvalidArgIdxs of
      [] -> "";
      [_] -> io_lib:format("They do not overlap in the ~ts argument", [ArgOrd]);
      [_|_] -> io_lib:format("They do not overlap in the ~ts arguments", [ArgOrd])
    end,
  RangeDesc =
    case IsRangeInvalid of
      true -> "return types do not overlap";
      false -> ""
    end,
  case {ArgDesc, RangeDesc} of
    {"", [_|_]} -> io_lib:format(" The ~ts\n", [RangeDesc]);
    {[_|_], ""} -> io_lib:format(" ~ts\n", [ArgDesc]);
    {[_|_], [_|_]} -> io_lib:format(" ~ts, and the ~ts\n", [ArgDesc, RangeDesc])
  end.


call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet,
			{IsOverloaded, Contract}, I) ->
  PositionString = form_position_string(ArgNs),
  case FailReason of
    only_sig ->
      case ArgNs =:= [] of
	true ->
	  %% We do not know which argument(s) caused the failure
	  io_lib:format("will never return since the success typing arguments"
			" are ~ts\n", [t(SigArgs, I)]);
        false ->
	  io_lib:format("will never return since it differs in the ~s argument"
			" from the success typing arguments: ~ts\n",
			[PositionString, t(SigArgs, I)])
      end;
    only_contract ->
      case (ArgNs =:= []) orelse IsOverloaded of
	true ->
	  %% We do not know which arguments caused the failure
	  io_lib:format("breaks the contract ~ts\n", [sig(Contract, I)]);
	false ->
	  io_lib:format("breaks the contract ~ts in the ~s argument\n",
			[sig(Contract, I), PositionString])
      end;
    both ->
      io_lib:format("will never return since the success typing is ~ts -> ~ts"
		    " and the contract is ~ts\n",
                    [t(SigArgs, I), t(SigRet, I), sig(Contract, I)])
  end.

form_positions(ArgNs) ->
  case ArgNs of
    [_] -> "an opaque term as ";
    [_,_|_] -> "opaque terms as "
  end
    ++ form_position_string(ArgNs)
    ++ case ArgNs of
         [_] -> " argument";
         [_,_|_] -> " arguments"
       end.

%% We know which positions N are to blame;
%% the list of triples will never be empty.
form_expected_without_opaque([{N, T, TStr}], I) ->
  case erl_types:t_is_opaque(T) of
    true ->
      io_lib:format("an opaque term of type ~ts as ", [t(TStr, I)]);
    false ->
      io_lib:format("a term of type ~ts (with opaque subterms) as ",
                    [t(TStr, I)])
  end ++ form_position_string([N]) ++ " argument";
form_expected_without_opaque(Conflicts, _I) -> %% TODO: can do much better here
  ArgNs = [N || {N, _T, _TStr} <- Conflicts],
  "opaque terms as " ++ form_position_string(ArgNs) ++ " arguments".

form_expected(ExpectedArgs, I) ->
  case ExpectedArgs of
    [T] ->
      TS = erl_types:t_to_string(T),
      case erl_types:t_is_opaque(T) of
	true  -> io_lib:format("an opaque term of type ~ts is expected",
                               [t(TS, I)]);
	false -> io_lib:format("a structured term of type ~ts is expected",
                               [t(TS, I)])
      end;
    [_,_|_] -> "terms of different types are expected in these positions"
  end.

form_position_string(ArgNs) ->
  case ArgNs of
    [] -> "";
    [N1] -> ordinal(N1);
    [_,_|_] ->
      [Last|Prevs] = lists:reverse(ArgNs),
      ", " ++ Head = lists:flatten([io_lib:format(", ~s",[ordinal(N)]) ||
				     N <- lists:reverse(Prevs)]),
      Head ++ " and " ++ ordinal(Last)
  end.

ordinal(N) when is_integer(N),
                ((N rem 100) =:= 11) orelse
                ((N rem 100) =:= 12) orelse
                ((N rem 100) =:= 13) ->
  io_lib:format("~Bth", [N]);
ordinal(N) when is_integer(N) ->
  case min(N rem 10, 4) of
    1 -> io_lib:format("~Bst", [N]);
    2 -> io_lib:format("~Bnd", [N]);
    3 -> io_lib:format("~Brd", [N]);
    _ -> io_lib:format("~Bth", [N])
  end.

%% Functions that parse type strings, literal strings, and contract
%% strings. Return strings formatted by erl_pp.

%% Note we always have to catch any error when trying to parse
%% the syntax because other BEAM languages may not emit an
%% Erlang AST that transforms into valid Erlang Source Code.

-define(IND, 10).

con(M, F, Src, I) ->
  S = sig(Src, I),
  io_lib:format("~w:~tw~ts", [M, F, S]).

sig(Src, false) ->
  Src;
sig(Src, true) ->
  try
    Str = lists:flatten(io_lib:format("-spec ~w:~tw~ts.", [a, b, Src])),
    {ok, Tokens, _EndLocation} = erl_scan:string(Str),
    {ok, {attribute, _, spec, {_MFA, Types}}} =
      erl_parse:parse_form(Tokens),
    indentation(?IND) ++ pp_spec(Types)
  catch
    _:_ -> Src
  end.

%% Argument(list)s are a mix of types and Erlang code. Note: sometimes
%% (contract_range, call_without_opaque, opaque_type_test), the initial
%% newline is a bit out of place.
a(""=Args, _I) ->
  Args;
a(Args, I) ->
  t(Args, I).

c(Cerl, _I) ->
  Cerl.

field_diffs(Src, false) ->
  Src;
field_diffs(Src, true) ->
  Fields = string:split(Src, " and ", all),
  lists:join(" and ", [field_diff(Field) || Field <- Fields]).

field_diff(Field) ->
  [F | Ts] = string:split(Field, "::", all),
  F ++ " ::" ++ t(lists:flatten(lists:join("::", Ts)), true).

rec_type("record "++Src, I) ->
  "record " ++ t(Src, I).

%% "variable"/"pattern" ++ cerl
ps("pattern "++Src, I) ->
  "pattern " ++ t(Src, I);
ps("variable "++_=Src, _I) ->
  Src;
ps("record field"++Rest, I) ->
  [S, TypeStr] = string:split(Rest, "of type ", all),
  "record field" ++ S ++ "of type " ++ t(TypeStr, I).

%% Scan and parse a type or a literal, and pretty-print it using erl_pp.
t(Src, false) ->
  Src;
t("("++_=Src, true) ->
  ts(Src);
t(Src, true) ->
  %% Binary types and products both start with a $<.
  try parse_type_or_literal(Src) of
    TypeOrLiteral ->
      indentation(?IND) ++ pp_type(TypeOrLiteral)
  catch
    _:_ ->
      ts(Src)
  end.

ts(Src) ->
  Ind = indentation(?IND),
  [C1|Src1] = Src, % $< (product) or $( (arglist)
  [C2|RevSrc2] = lists:reverse(Src1),
  Src2 = lists:reverse(RevSrc2),
  try
    Types = parse_types_and_literals(Src2),
    CommaInd = [$, | Ind],
    (indentation(?IND-1) ++
     [C1 | lists:join(CommaInd, [pp_type(Type) || Type <- Types])] ++
     [C2])
  catch
    _:_ -> Src
  end.

indentation(I) ->
  [$\n | lists:duplicate(I, $\s)].

pp_type(Type) ->
  Form = {attribute, erl_anno:new(0), type, {t, Type, []}},
  TypeDef = erl_pp:form(Form, [{quote_singleton_atom_types, true}]),
  {match, [S]} = re:run(TypeDef, <<"::\\s*(.*)\\.\\n*">>,
                        [{capture, all_but_first, list}, dotall, unicode]),
  S.

pp_spec(Spec) ->
  Form = {attribute, erl_anno:new(0), spec, {{a,b,0}, Spec}},
  Sig = erl_pp:form(Form, [{quote_singleton_atom_types, true}]),
  {match, [S]} = re:run(Sig, <<"-spec a:b\\s*(.*)\\.\\n*">>,
                        [{capture, all_but_first, list}, dotall, unicode]),
  S.

parse_types_and_literals(Src) ->
  {ok, Tokens, _EndLocation} = erl_scan:string(Src),
  [parse_a_type_or_literal(Ts) || Ts <- types(Tokens)].

parse_type_or_literal(Src) ->
  {ok, Tokens, _EndLocation} = erl_scan:string(Src),
  parse_a_type_or_literal(Tokens).

parse_a_type_or_literal(Ts0) ->
  L = erl_anno:new(1),
  Ts = Ts0 ++ [{dot,L}],
  Tokens = [{'-',L}, {atom,L,type}, {atom,L,t}, {'(',L}, {')',L},
            {'::',L}] ++ Ts,
  case erl_parse:parse_form(Tokens) of
      {ok, {attribute, _, type, {t, Type, []}}} ->
          Type;
      {error, _} ->
          %% literal
          {ok, [T]} = erl_parse:parse_exprs(Ts),
          T
  end.

types([]) -> [];
types(Ts) ->
    {Ts0, Ts1} = one_type(Ts, [], []),
    [Ts0 | types(Ts1)].

one_type([], [], Ts) ->
    {lists:reverse(Ts), []};
one_type([{',', _Lc}|Toks], [], Ts0) ->
    {lists:reverse(Ts0), Toks};
one_type([{')', Lrp}|Toks], [], Ts0) ->
    {lists:reverse(Ts0), [{')', Lrp}|Toks]};
one_type([{'(', Llp}|Toks], E, Ts0) ->
    one_type(Toks, [')'|E], [{'(', Llp}|Ts0]);
one_type([{'<<', Lls}|Toks], E, Ts0) ->
    one_type(Toks, ['>>'|E], [{'<<', Lls}|Ts0]);
one_type([{'[', Lls}|Toks], E, Ts0) ->
    one_type(Toks, [']'|E], [{'[', Lls}|Ts0]);
one_type([{'{', Llc}|Toks], E, Ts0) ->
    one_type(Toks, ['}'|E], [{'{', Llc}|Ts0]);
one_type([{Rb, Lrb}|Toks], [Rb|E], Ts0) ->
    one_type(Toks, E, [{Rb, Lrb}|Ts0]);
one_type([T|Toks], E, Ts0) ->
    one_type(Toks, E, [T|Ts0]).
