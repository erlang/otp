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

-module(dialyzer_cl_parse).
-moduledoc false.

-export([start/0]).

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------

-type dial_cl_parse_ret() :: {'check_init', #options{}}
                           | {'plt_info', #options{}}
                           | {'cl', #options{}}
                           | {'error', string()}.

-spec start() -> dial_cl_parse_ret().
start() ->
    Args = init:get_plain_arguments(),
    try argparse:parse(Args, cli(), #{progname => dialyzer}) of
        {ok, ArgMap, _, _} ->
            {Command, Opts} = postprocess_side_effects(ArgMap),
            case dialyzer_options:build(maps:to_list(Opts)) of
                {error, Msg2} ->
                    {error, Msg2};
                OptsRecord ->
                    {Command, OptsRecord}
            end;
        {error, Error} ->
            {error, argparse:format_error(Error)}
    catch
        throw:{dialyzer_cl_parse_error, Msg} ->
            {error, Msg};
        _:R:S ->
            Msg = io_lib:format("~tp\n~tp\n", [R, S]),
            {error, lists:flatten(Msg)}
    end.

%%-----------------------------------------------------------------------

parse_app(AppOrDir) ->
    case code:lib_dir(list_to_atom(AppOrDir)) of
        {error, bad_name} -> AppOrDir;
        LibDir -> filename:join(LibDir, "ebin")
    end.

parse_input_list(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Files = binary:split(Bin, <<"\n">>, [trim_all, global]),
            [binary_to_list(string:trim(F)) || F <- Files];
        {error, Reason} ->
            cl_error(io_lib:format("Reading of ~s failed: ~s", [File, file:format_error(Reason)]))
    end.

parse_define(Arg) ->
    case re:split(Arg, "=", [{return, list}, unicode]) of
        [Def, Val] ->
            {ok, Tokens, _} = erl_scan:string(Val++"."),
            {ok, ErlVal} = erl_parse:parse_term(Tokens),
            {list_to_atom(Def), ErlVal};
        [Def] ->
            {list_to_atom(Def), true}
    end.

cli() ->
    #{
        arguments => [
            #{name => files, action => extend, nargs => list, required => false,
                help => <<"Use Dialyzer from the command line to detect defects in the "
                        "specified files or directories containing .erl or .beam files, "
                        "depending on the type of the analysis.">>},
            #{name => files, short => $c, long => "-com", action => extend, nargs => list,
                help => <<"Same as files, specifies files to run the analysis on (left for compatibility)">>},
            #{name => files_rec, short => $r, action => extend, nargs => list,
                help => <<"Search the specified directories "
                        "recursively for subdirectories containing .erl or .beam files in "
                        "them, depending on the type of analysis.">>},
            #{name => files, long => "-input_list_file", type => {custom, fun parse_input_list/1},
                action => extend,
                help => <<"Specify the name of a file that contains the names of the files "
                        "to be analyzed (one file name per line).">>},
            #{name => files_rec, long => "-apps", type => {custom, fun parse_app/1},
                nargs => list, action => extend,
                help => <<"Option typically used when building or modifying a plt as in: \n"
                        "dialyzer --build_plt --apps erts kernel stdlib mnesia ... \n"
                        "to conveniently refer to library applications corresponding to the "
                        "Erlang/OTP installation. However, the option is general and can also "
                        "be used during analysis in order to refer to Erlang/OTP applications. "
                        "In addition, file or directory names can also be included, as in: \n"
                        "dialyzer --apps inets ssl ./ebin ../other_lib/ebin/my_module.beam">>},

            #{name => output_file, short => $o, long => "-output",
                help => <<"When using Dialyzer from the command line, send the analysis "
                        "results to the specified outfile rather than to stdout.">>},
            #{name => output_format, long => "-raw", type => boolean, action => {store, raw},
                help => <<"When using Dialyzer from the command line, output the raw analysis "
                        "results (Erlang terms) instead of the formatted result. "
                        "The raw format is easier to post-process (for instance, to filter "
                        "warnings or to output HTML pages).">>},
            #{name => from, long => "-src", type => boolean, action => {store, src_code},
                help => <<"Override the default, which is to analyze BEAM files, and "
                        "analyze starting from Erlang source code instead.">>},
            #{name => defines, short=>$D, type => {custom, fun parse_define/1}, action => append,
                help => <<"When analyzing from source, pass the define to Dialyzer. (**)">>},
            #{name => include_dirs, short=>$I, action => append,
                help => <<"When analyzing from source, pass the include_dir to Dialyzer. (**)">>},
            #{name => pa, long => "pa", action => append,
                help => <<"Include dir in the path for Erlang (useful when analyzing files "
                        "that have '-include_lib()' directives).">>},
            #{name => output_plt, long => "-output_plt",
                help => <<"Store the plt at the specified file after building it.">>},
            #{name => plts, long => "-plt", nargs => 1,
                help => <<"Use the specified plt as the initial plt (if the plt was built "
                        "during setup the files will be checked for consistency).">>},
            #{name => plts, long => "-plts", nargs => nonempty_list,
                help => <<"Merge the specified plts to create the initial plt -- requires "
                        "that the plts are disjoint (i.e., do not have any module "
                        "appearing in more than one plt). "
                        "The plts are created in the usual way: \n"
                        "  dialyzer --build_plt --output_plt plt_1 files_to_include "
                        "  ... \n"
                        "  dialyzer --build_plt --output_plt plt_n files_to_include "
                        "and then can be used in either of the following ways: \n"
                        "  dialyzer files_to_analyze --plts plt_1 ... plt_n \n"
                        "or: \n"
                        "  dialyzer --plts plt_1 ... plt_n -- files_to_analyze \n"
                        "(Note the -- delimiter in the second case)">>},
            #{name => warnings, short => $W, action => append, type => {atom, [error_handling,
                no_behaviours, no_contracts, no_fail_call, no_fun_app, no_improper_lists,
                no_match, no_missing_calls, no_opaque, no_return, no_undefined_callbacks,
                no_underspecs, no_unknown, no_unused, underspecs, unknown, unmatched_returns,
                overspecs, specdiffs, overlapping_contract, extra_return, no_extra_return, missing_return,
                no_missing_return, opaque_union]},
                help => {<<"[-Wwarn]*">>, [<<"A family of options which selectively turn on/off warnings">>]}},
            #{name => version, short => $v, long => "-version", type => boolean,
                help => <<"Print the Dialyzer version and some more information and exit.">>},
            #{name => help, short => $h, long => "-help", type => boolean,
                help => <<"Print this message and exit.">>},
            #{name => report_mode, short => $q, long => "-quiet", type => boolean, action => {store, quiet},
                default => normal, help => <<"Make Dialyzer a bit more quiet.">>},
            #{name => report_mode, long => "-verbose", type => boolean, action => {store, verbose},
                help => <<"Make Dialyzer a bit more verbose.">>},
            #{name => timing, long => "-statistics", type => boolean,
                help => <<"Prints information about the progress of execution (analysis phases, "
                        "time spent in each and size of the relative input).">>},
            #{name => analysis_type, long => "-build_plt", type => boolean, action => {store, plt_build},
                help => <<"The analysis starts from an empty plt and creates a new one from the "
                        "files specified with -c and -r. Only works for beam files. "
                        "Use --plt(s) or --output_plt to override the default plt location.">>},
            #{name => analysis_type, long=> "-add_to_plt", type => boolean, action => {store, plt_add},
                help => <<"The plt is extended to also include the files specified with -c and -r. "
                        "Use --plt(s) to specify which plt to start from, and --output_plt to "
                        "specify where to put the plt. Note that the analysis might include "
                        "files from the plt if they depend on the new files. "
                        "This option only works with beam files.">>},
            #{name => analysis_type, long => "-remove_from_plt", type => boolean, action => {store, plt_remove},
                help => <<"The information from the files specified with -c and -r is removed "
                        "from the plt. Note that this may cause a re-analysis of the remaining "
                        "dependent files.">>},
            #{name => analysis_type, long => "-check_plt", type => boolean, action => {store, plt_check},
                help => <<"Check the plt for consistency and rebuild it if it is not up-to-date. "
                        "Actually, this option is of rare use as it is on by default.">>},
            #{name => check_plt, long => "-no_check_plt", short => $n, type => boolean, action => {store, false},
                help => <<"Skip the plt check when running Dialyzer. Useful when working with "
                        "installed plts that never change.">>},
            #{name => analysis_type, long => "-incremental", type => boolean, action => {store, incremental},
                help => <<"The analysis starts from an existing incremental PLT, or builds one from "
                        "scratch if one doesn't exist, and runs the minimal amount of additional "
                        "analysis to report all issues in the given set of apps. Notably, incremental "
                        "PLT files are not compatible with \"classic\" PLT files, and vice versa. "
                        "The initial incremental PLT will be updated unless an alternative output "
                        "incremental PLT is given.">>},
            #{name => analysis_type, long => "-plt_info", type => boolean, action => {store, plt_info},
                help => <<"Make Dialyzer print information about the plt and then quit. The plt "
                        "can be specified with --plt(s).">>},
            #{name => get_warnings, long => "-get_warnings", type => boolean,
                help => <<"Make Dialyzer emit warnings even when manipulating the plt. Warnings "
                        "are only emitted for files that are actually analyzed.">>},
            #{name => callgraph_file, long => "-dump_callgraph",
                help => <<"Dump the call graph into the specified file whose format is determined "
                        "by the file name extension. Supported extensions are: raw, dot, and ps. "
                        "If something else is used as file name extension, default format '.raw' "
                        "will be used.">>},
            #{name => mod_deps_file, long => "-dump_full_dependencies_graph",
                help => <<"Dump the full dependency graph (i.e. dependencies induced by function "
                        "calls, usages of types in specs, behaviour implementations, etc.) into "
                        "the specified file whose format is determined by the file name "
                        "extension. Supported extensions are: dot and ps.">>},
            #{name => error_location, long => "-error_location", type => {atom, [column, line]},
                help => <<"Use a pair {Line, Column} or an integer Line to pinpoint the location "
                        "of warnings. The default is to use a pair {Line, Column}. When "
                        "formatted, the line and the column are separated by a colon.">>},
            #{name => filename_opt, long => "-fullpath", type => boolean, action => {store, fullpath},
                help => <<"Display the full path names of files for which warnings are emitted.">>},
            #{name => indent_opt, long => "-no_indentation", type => boolean, action => {store, false},
                help => <<"Do not indent contracts and success typings. Note that this option has "
                        "no effect when combined with the --raw option.">>},
            #{name => metrics_file, long => "-metrics_file",
                help => <<"Write metrics about Dialyzer's incrementality (for example, total number of "
                        "modules considered, how many modules were changed since the PLT was "
                        "last updated, how many modules needed to be analyzed) to a file. This "
                        "can be useful for tracking and debugging Dialyzer's incrementality.">>},
            #{name => warning_files_rec, long => "-warning_apps", type => {custom, fun parse_app/1},
                nargs => list, action => extend,
                help => <<"By default, warnings will be reported to all applications given by "
                        "--apps. However, if --warning_apps is used, only those applications "
                        "given to --warning_apps will have warnings reported. All applications "
                        "given by --apps, but not --warning_apps, will be analysed to provide "
                        "context to the analysis, but warnings will not be reported for them. "
                        "For example, you may want to include libraries you depend on in the "
                        "analysis with --apps so discrepancies in their usage can be found, "
                        "but only include your own code with --warning_apps so that "
                        "discrepancies are only reported in code that you own.">>},

            %% Intentionally undocumented options
            #{name => solvers, long => "-solver", type => {atom, [v1, v2]}, action => append,
                help => hidden},
            #{name => timing, long => "-resources", type => boolean, action => {store, debug},
                help => hidden},

            %% next definition is necessary to ignore '--' left for compatibility reasons
            #{name => shell, short => $-, type => boolean, help => hidden}
        ],

        help => [<<"Usage: ">>, usage, <<"\n\nOptions:\n">>,
            arguments, options, "
Note:
  * denotes that multiple occurrences of these options are possible.
 ** the syntax of defines and includes is the same as that used by \"erlc\".

" ++ warning_options_msg() ++ "
" ++ configuration_file_msg() ++ "

The exit status of the command line version is:
  0 - No problems were encountered during the analysis and no
      warnings were emitted.
  1 - Problems were encountered during the analysis.
  2 - No problems were encountered, but warnings were emitted.

"]
    }.

postprocess_side_effects(ArgMap) when is_map_key(version, ArgMap) ->
    %% Version handling
    io:format("Dialyzer version " ++ ?VSN ++ "\n"),
    erlang:halt(?RET_NOTHING_SUSPICIOUS);

postprocess_side_effects(ArgMap) when is_map_key(help, ArgMap) ->
    %% Help message
    io:format(argparse:help(cli(), #{progname => dialyzer})),
    erlang:halt(?RET_NOTHING_SUSPICIOUS);

postprocess_side_effects(ArgMap) when is_map_key(pa, ArgMap) ->
    %% Code path side effect
    [code:add_patha(Path) =/= true andalso cl_error("Bad directory for -pa: " ++ Path) ||
        Path <- map_get(pa, ArgMap)],
    postprocess_side_effects(maps:remove(pa, ArgMap));

postprocess_side_effects(ArgMap) when is_map_key(shell, ArgMap) ->
    %% --shell option is processed by C executable (left here only for help/usage)
    postprocess_side_effects(maps:remove(shell, ArgMap));

postprocess_side_effects(ArgMap) ->
    %% if all files specified are ".erl" files, set the 'src' flag automatically
    %% it is compatibility behaviour, potentially incorrect, because it does not take
    %% directories (rec_files) into account
    ArgMap1 =
        case (is_map_key(files, ArgMap) andalso
            lists:all(fun(F) -> filename:extension(F) =:= ".erl" end, maps:get(files, ArgMap))) of
            true ->
                ArgMap#{from => src_code};
            false ->
                ArgMap
        end,

    %% Run mode (command) is defined by the flag combination
    case maps:get(analysis_type, ArgMap1, undefined) of
        plt_info ->
            %% plt_info is plt_check analysis type
            {plt_info, ArgMap1#{analysis_type => plt_check}};
        plt_check ->
            %% plt_check is a hidden "check_init" command
            {check_init, ArgMap1};
        _ ->
            {cl, ArgMap1}
    end.

cl_error(Str) ->
    Msg = lists:flatten(Str),
    throw({dialyzer_cl_parse_error, Msg}).

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
  -Wno_unknown
     Suppress warnings about unknown functions and types. The default is to
     warn about unknown functions and types when setting the exit
     status. When using Dialyzer from Erlang, warnings about unknown functions
     and types are returned.
  -Wunknown
     Warns about unknown functions and types when setting the exit
     status (enabled by default). When using Dialyzer from Erlang, warnings about unknown functions
     and types are returned.
  -Wunmatched_returns ***
     Include warnings for function calls which ignore a structured return
     value or do not match against one of many possible return value(s).
  -Werror_handling ***
     Include warnings for functions that only return by means of an exception.
  -Wunderspecs ***
     Warn about underspecified functions
     (those whose -spec is strictly more allowing than the success typing).
  -Wextra_return ***
     Warn about functions whose specification includes types that the
     function cannot return.
  -Wmissing_return ***
     Warn about functions that return values that are not part
     of the specification.
  -Woverlapping_contract ***
     Warn about overloaded functions whose specification include types that
     overlap.
  -Wopaque_union ***
     Warn about potentially creating a union between opaques and non-opaques.

The following options are also available but their use is not recommended:
(they are mostly for Dialyzer developers and internal debugging)
  -Woverspecs ***
     Warn about overspecified functions
     (those whose -spec is strictly less allowing than the success typing).
  -Wspecdiffs ***
     Warn when the -spec is different than the success typing.

*** Identifies options that turn on warnings rather than turning them off.

The following options are not strictly needed as they specify the default.
They are primarily intended to be used with the -dialyzer attribute:
  -Wno_underspecs
     Suppress warnings about underspecified functions (those whose -spec
     is strictly more allowing than the success typing).
  -Wno_extra_return
     Suppress warnings about functions whose specification includes types that the function cannot return.
  -Wno_missing_return
     Suppress warnings about functions that return values that are not part of the specification.
".

configuration_file_msg() ->
    "Configuration file:
     Dialyzer's configuration file may also be used to augment the default
     options and those given directly to the Dialyzer command. It is commonly
     used to avoid repeating options which would otherwise need to be given
     explicitly to Dialyzer on every invocation.

     The location of the configuration file can be set via the
     DIALYZER_CONFIG environment variable, and defaults to
     within the user_config location given by filename:basedir/3.

     On your system, the location is currently configured as:
       " ++ dialyzer_options:get_default_config_filename() ++
     "

     An example configuration file's contents might be:

       {incremental,
         {default_apps,[stdlib,kernel,erts]},
         {default_warning_apps,[stdlib]}
       }.
       {warnings, [no_improper_lists]}.
       {add_pathsa,[\"/users/samwise/potatoes/ebin\"]}.
       {add_pathsz,[\"/users/smeagol/fish/ebin\"]}.
".
