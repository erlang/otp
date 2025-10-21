%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004 Richard Carlsson
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
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @doc Provides a better way to start Dialyzer from a script.

-module(dialyzer_options).
-moduledoc false.

-export([build/1, build_warnings/2, get_default_config_filename/0]).

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------

-spec build(Options) -> #options{} | {'error', string()} when
    Options :: [Option],
    Option :: dial_option()
            | {'report_mode', rep_mode()}
            | {'erlang_mode', boolean()}.

build(Opts) ->
  DefaultWarns = [?WARN_RETURN_NO_RETURN,
                  ?WARN_NOT_CALLED,
                  ?WARN_NON_PROPER_LIST,
                  ?WARN_FUN_APP,
                  ?WARN_MATCHING,
                  ?WARN_OPAQUE,
                  ?WARN_CALLGRAPH,
                  ?WARN_FAILING_CALL,
                  ?WARN_BIN_CONSTRUCTION,
                  ?WARN_MAP_CONSTRUCTION,
                  ?WARN_CONTRACT_OPAQUE,
                  ?WARN_CONTRACT_RANGE,
                  ?WARN_CONTRACT_TYPES,
                  ?WARN_CONTRACT_SYNTAX,
                  ?WARN_BEHAVIOUR,
                  ?WARN_UNDEFINED_CALLBACK,
                  ?WARN_UNKNOWN],
  DefaultWarns1 = ordsets:from_list(DefaultWarns),
  try
    WarningsFromConfig = proplists:get_value(warnings, get_config(), []),
    update_path_from_config(),
    DefaultWarns2 = build_warnings(WarningsFromConfig, DefaultWarns1),
    DefaultOpts = #options{},
    DefaultOpts1 = DefaultOpts#options{legal_warnings = DefaultWarns2},
    Opts1 = preprocess_opts(Opts),
    Env = env_default_opts(),
    ErrLoc = proplists:get_value(error_location, Env, ?ERROR_LOCATION),
    EnvOpts = [{error_location, ErrLoc}],
    NewOpts = build_options(EnvOpts ++ Opts1, DefaultOpts1),
    postprocess_opts(NewOpts)
  catch
    throw:{dialyzer_options_error, Msg} -> {error, Msg}
  end.

update_path_from_config() ->
  Config = get_config(),
  PAs = proplists:get_value(add_pathsa, Config, []),
  PZs = proplists:get_value(add_pathsz, Config, []),
  case is_list(PAs) of
    true -> ok;
    false -> bad_option("Bad list of paths in config", {add_pathsa, PAs})
  end,
  case is_list(PZs) of
    true -> ok;
    false -> bad_option("Bad list of paths in config", {add_pathsz, PZs})
  end,
  %% Add paths one-by-one so that we can report issues
  %% if any path is invalid
  %% (code:add_pathsa/1 and code:add_pathsz/1 always return ok)
  [ case code:add_patha(PA) of
      true -> ok;
      {error, _} -> bad_option("Failed to add path from config", {add_patha, PA})
    end || PA <- PAs ],
  [ case code:add_pathz(PZ) of
      true -> ok;
      {error, _} -> bad_option("Failed to add path from config", {add_pathz, PZ})
    end || PZ <- PZs ],
  ok.

preprocess_opts([]) -> [];
preprocess_opts([{init_plt, File}|Opts]) ->
  [{plts, [File]}|preprocess_opts(Opts)];
preprocess_opts([Opt|Opts]) ->
  [Opt|preprocess_opts(Opts)].

postprocess_opts(Opts = #options{}) ->
  Opts1 =
    case {Opts#options.init_plts, Opts#options.analysis_type} of
      {[],incremental} -> Opts#options{init_plts=[dialyzer_iplt:get_default_iplt_filename()]};
      {[],_} -> Opts#options{init_plts=[dialyzer_cplt:get_default_cplt_filename()]};
      {[_|_],_} -> Opts
    end,
  check_file_existence(Opts1),
  check_metrics_file_validity(Opts1),
  check_module_lookup_file_validity(Opts1),
  Opts2 = check_output_plt(Opts1),
  check_init_plt_kind(Opts2),
  Opts3 = manage_default_incremental_apps(Opts2),
  adapt_get_warnings(Opts3).

check_metrics_file_validity(#options{analysis_type = incremental, metrics_file = none}) ->
  ok;
check_metrics_file_validity(#options{analysis_type = incremental, metrics_file = FileName}) ->
  assert_filename(FileName);
check_metrics_file_validity(#options{analysis_type = _NotIncremental, metrics_file = none}) ->
  ok;
check_metrics_file_validity(#options{analysis_type = _NotIncremental, metrics_file = FileName}) ->
  bad_option("A metrics filename may only be given when in incremental mode", {metrics_file, FileName}).

check_module_lookup_file_validity(#options{analysis_type = incremental, module_lookup_file = none}) ->
  ok;
check_module_lookup_file_validity(#options{analysis_type = incremental, module_lookup_file = FileName}) ->
  assert_filename(FileName);
check_module_lookup_file_validity(#options{analysis_type = _NotIncremental, module_lookup_file = none}) ->
  ok;
check_module_lookup_file_validity(#options{analysis_type = _NotIncremental, module_lookup_file = FileName}) ->
  bad_option("A module lookup filename may only be given when in incremental mode", {module_lookup_file, FileName}).

check_file_existence(#options{analysis_type = plt_remove}) -> ok;
check_file_existence(#options{files = Files, files_rec = FilesRec,
  warning_files = WarningFiles, warning_files_rec = WarningFilesRec}) ->
  assert_filenames_exist(Files),
  assert_filenames_exist(FilesRec),
  assert_filenames_exist(WarningFiles),
  assert_filenames_exist(WarningFilesRec).

check_output_plt(Opts = #options{analysis_type = Mode, from = From,
				 output_plt = OutPLT}) ->
  case is_plt_mode(Mode) of
    true ->
      case From =:= byte_code of
	true -> Opts;
	false ->
	  Msg = "Byte code compiled with debug_info is needed to build the PLT",
	  throw({dialyzer_error, Msg})
      end;
    false ->
      case OutPLT =:= none of
	true -> Opts;
	false ->
	  Msg = io_lib:format("Output PLT cannot be specified "
			      "in analysis mode ~w", [Mode]),
	  throw({dialyzer_error, lists:flatten(Msg)})
      end
  end.

check_init_plt_kind(#options{analysis_type = incremental, init_plts = InitPlts}) ->
  RunCheck = fun(FileName) ->
                 case dialyzer_plt:plt_kind(FileName) of
                   no_file -> ok;
                   iplt -> ok;
                   cplt ->
                     bad_option("Given file is a classic PLT file, "
                                "but in incremental mode, "
                                "an incremental PLT file is expected",
                                {init_plt_file, FileName});
                   bad_file ->
                     bad_option("Given file is not a PLT file", {init_plt_file, FileName})
                 end
             end,
  lists:foreach(RunCheck, InitPlts);
check_init_plt_kind(#options{analysis_type = _NotIncremental, init_plts = InitPlts}) ->
  RunCheck = fun(FileName) ->
                 case dialyzer_plt:plt_kind(FileName) of
                   no_file -> ok;
                   cplt -> ok;
                   iplt ->
                     bad_option("Given file is an incremental PLT file, "
                                "but outside of incremental mode, "
                                "a classic PLT file is expected",
                                {init_plt_file, FileName});
                   bad_file ->
                     bad_option("Given file is not a PLT file", {init_plt_file, FileName})
                 end
             end,
  lists:foreach(RunCheck, InitPlts).

%% If no apps are set explicitly, we fall back to config
manage_default_incremental_apps(Opts = #options{analysis_type = incremental, files = [], files_rec = [], warning_files = [], warning_files_rec = []}) ->
  set_default_apps(get_config(), Opts);
manage_default_incremental_apps(Opts) ->
  Opts.

set_default_apps([ConfigElem|MoreConfig], Opts) ->
  case ConfigElem of
    {incremental, {default_apps, DefaultApps}=Term} when
      is_list(DefaultApps) ->
        AppDirs = get_app_dirs(DefaultApps),
        assert_filenames_form(Term, AppDirs),
        Opts#options{files_rec = AppDirs};
    {incremental, {default_apps, DefaultApps}=TermApps,
                  {default_warning_apps, DefaultWarningApps}=TermWarns} when
      is_list(DefaultApps), is_list(DefaultWarningApps) ->
        AppDirs = get_app_dirs(DefaultApps ++ DefaultWarningApps),
        assert_filenames_form(TermApps, AppDirs),
        WarningAppDirs = get_app_dirs(DefaultWarningApps),
        assert_filenames_form(TermWarns, WarningAppDirs),
        Opts#options{files_rec = AppDirs, warning_files_rec = WarningAppDirs};
    _ when element(1, ConfigElem) =:= incremental ->
      bad_option("Given Erlang terms in 'incremental' section could not be understood as Dialyzer config", ConfigElem);
    _ ->
      set_default_apps(MoreConfig, Opts)
  end;
set_default_apps([], Opts) ->
  Opts.

get_config() ->
  DefaultConfig = get_default_config_filename(),
  case filelib:is_regular(DefaultConfig) of
    true ->
      case file:consult(DefaultConfig) of
        {ok, Config} when is_list(Config) -> Config;
        {error, Reason} ->
          bad_option(file:format_error(Reason), DefaultConfig)
      end;
    false ->
      []
  end.

% Intended to work like dialyzer_iplt:get_default_iplt_filename()
-spec get_default_config_filename() -> string().
get_default_config_filename() ->
  case os:getenv("DIALYZER_CONFIG") of
     false ->
       CacheDir = filename:basedir(user_config, "erlang"),
       filename:join(CacheDir, "dialyzer.config");
     UserSpecConfig -> UserSpecConfig
  end.

adapt_get_warnings(Opts = #options{analysis_type = Mode,
				   get_warnings = Warns}) ->
  %% Warnings are off by default in plt mode, and on by default in
  %% success typings mode. User defined warning mode overrides the
  %% default.
  case is_plt_mode(Mode) of
    true ->
      case Warns =:= 'maybe' of
	true -> Opts#options{get_warnings = false};
	false -> Opts
      end;
    false ->
      case Warns =:= 'maybe' of
	true -> Opts#options{get_warnings = true};
	false -> Opts
      end
  end.

-spec bad_option(string(), term()) -> no_return().

bad_option(String, Term) ->
  Msg = io_lib:format("~ts: ~tP", [String, Term, 25]),
  throw({dialyzer_options_error, lists:flatten(Msg)}).

build_options([{OptName, undefined}|Rest], Options) when is_atom(OptName) ->
  build_options(Rest, Options);
build_options([{OptionName, Value} = Term|Rest], Options) ->
  case OptionName of
    apps ->
      OldValues = Options#options.files_rec,
      AppDirs = get_app_dirs(Value),
      assert_filenames_form(Term, AppDirs),
      build_options(Rest, Options#options{files_rec = AppDirs ++ OldValues});
    files ->
      assert_filenames_form(Term, Value),
      build_options(Rest, Options#options{files = Value});
    files_rec ->
      OldValues = Options#options.files_rec,
      assert_filenames_form(Term, Value),
      build_options(Rest, Options#options{files_rec = Value ++ OldValues});
    warning_apps ->
      OldValues = Options#options.warning_files_rec,
      AppDirs = get_app_dirs(Value),
      assert_filenames_form(Term, AppDirs),
      build_options(Rest, Options#options{warning_files_rec = AppDirs ++ OldValues});
    warning_files ->
      assert_filenames_form(Term, Value),
      build_options(Rest, Options#options{warning_files = Value});
    warning_files_rec ->
      OldValues = Options#options.warning_files_rec,
      assert_filenames_form(Term, Value),
      build_options(Rest, Options#options{warning_files_rec = Value ++ OldValues});
    analysis_type ->
      NewOptions =
	case Value of
	  succ_typings -> Options#options{analysis_type = Value};
	  plt_add      -> Options#options{analysis_type = Value};
	  plt_build    -> Options#options{analysis_type = Value};
	  plt_check    -> Options#options{analysis_type = Value};
	  plt_remove   -> Options#options{analysis_type = Value};
    incremental -> Options#options{analysis_type = Value};
	  dataflow  -> bad_option("Analysis type is no longer supported", Term);
	  old_style -> bad_option("Analysis type is no longer supported", Term);
	  Other     -> bad_option("Unknown analysis type", Other)
	end,
      assert_plt_op(Options, NewOptions),
      build_options(Rest, NewOptions);
    check_plt when is_boolean(Value) ->
      build_options(Rest, Options#options{check_plt = Value});
    defines ->
      assert_defines(Term, Value),
      OldVal = Options#options.defines,
      NewVal = ordsets:union(ordsets:from_list(Value), OldVal),
      build_options(Rest, Options#options{defines = NewVal});
    from when Value =:= byte_code; Value =:= src_code ->
      build_options(Rest, Options#options{from = Value});
    get_warnings ->
      build_options(Rest, Options#options{get_warnings = Value});
    plts ->
      %assert_filenames(Term, Value),
      build_options(Rest, Options#options{init_plts = Value});
    include_dirs ->
      assert_filenames(Term, Value),
      OldVal = Options#options.include_dirs,
      NewVal = ordsets:union(ordsets:from_list(Value), OldVal),
      build_options(Rest, Options#options{include_dirs = NewVal});
    use_spec when is_boolean(Value) ->
      build_options(Rest, Options#options{use_contracts = Value});
    no_spec when is_boolean(Value) ->
      build_options(Rest, Options#options{use_contracts = not Value});
    old_style ->
      bad_option("Analysis type is no longer supported", old_style);
    output_file ->
      assert_filename(Value),
      build_options(Rest, Options#options{output_file = Value});
    metrics_file ->
      assert_filename(Value),
      build_options(Rest, Options#options{metrics_file = Value});
    module_lookup_file ->
      assert_filename(Value),
      build_options(Rest, Options#options{module_lookup_file = Value});
    output_format ->
      assert_output_format(Value),
      build_options(Rest, Options#options{output_format = Value});
    filename_opt ->
      assert_filename_opt(Value),
      build_options(Rest, Options#options{filename_opt = Value});
    indent_opt ->
      build_options(Rest, Options#options{indent_opt = Value});
    output_plt ->
      assert_filename(Value),
      build_options(Rest, Options#options{output_plt = Value});
    report_mode ->
      build_options(Rest, Options#options{report_mode = Value});
    erlang_mode ->
      build_options(Rest, Options#options{erlang_mode = true});
    warnings ->
      NewWarnings = build_warnings(Value, Options#options.legal_warnings),
      build_options(Rest, Options#options{legal_warnings = NewWarnings});
    callgraph_file ->
      assert_filename(Value),
      build_options(Rest, Options#options{callgraph_file = Value});
    mod_deps_file ->
      assert_filename(Value),
      build_options(Rest, Options#options{mod_deps_file = Value});
    error_location ->
      assert_error_location(Value),
      build_options(Rest, Options#options{error_location = Value});
    timing ->
      build_options(Rest, Options#options{timing = Value});
    solvers ->
      assert_solvers(Value),
      build_options(Rest, Options#options{solvers = Value});
    native ->
      %% Ignored since Erlang/OTP 24.0.
      build_options(Rest, Options);
    native_cache ->
      %% Ignored since Erlang/OTP 24.0.
      build_options(Rest, Options);
    _ ->
      bad_option("Unknown dialyzer command line option", Term)
  end;
build_options([], Options) ->
  Options.

get_app_dirs(Apps) when is_list(Apps) ->
  get_lib_dir([atom_to_list(A) || A <- Apps]);
get_app_dirs(Apps) ->
  bad_option("Use a list of otp applications", Apps).

get_lib_dir(Apps) ->
    get_lib_dir(Apps, []).

get_lib_dir([H|T], Acc) ->
    NewElem =
        case code:lib_dir(list_to_atom(H)) of
            {error, bad_name} -> H;
            LibDir -> filename:join(LibDir,"ebin")
        end,
    get_lib_dir(T, [NewElem|Acc]);
get_lib_dir([], Acc) ->
    lists:reverse(Acc).

assert_filenames(Term, Files) ->
  assert_filenames_form(Term, Files),
  assert_filenames_exist(Files).

assert_filenames_form(Term, [FileName|Left]) when length(FileName) >= 0 ->
  assert_filenames_form(Term, Left);
assert_filenames_form(_Term, []) ->
  ok;
assert_filenames_form(Term, [_|_]) ->
  bad_option("Malformed or non-existing filename", Term).

assert_filenames_exist([FileName|Left]) ->
  case filelib:is_file(FileName) orelse filelib:is_dir(FileName) of
    true -> ok;
    false ->
      bad_option("No such file, directory or application", FileName)
  end,
  assert_filenames_exist(Left);
assert_filenames_exist([]) ->
  ok.

assert_filename(FileName) when length(FileName) >= 0 ->
  ok;
assert_filename(FileName) ->
  bad_option("Malformed or non-existing filename", FileName).

assert_defines(Term, [{Macro, _Value}|Defs]) when is_atom(Macro) ->
  assert_defines(Term, Defs);
assert_defines(_Term, []) ->
  ok;
assert_defines(Term, [_|_]) ->
  bad_option("Malformed define", Term).

assert_output_format(raw) ->
  ok;
assert_output_format(formatted) ->
  ok;
assert_output_format(Term) ->
  bad_option("Illegal value for output_format", Term).

assert_filename_opt(basename) ->
  ok;
assert_filename_opt(fullpath) ->
  ok;
assert_filename_opt(Term) ->
  bad_option("Illegal value for filename_opt", Term).

assert_plt_op(#options{analysis_type = OldVal},
	      #options{analysis_type = NewVal}) ->
  case is_plt_mode(OldVal) andalso is_plt_mode(NewVal) of
    true -> bad_option("Options cannot be combined", [OldVal, NewVal]);
    false -> ok
  end.

is_plt_mode(plt_add)      -> true;
is_plt_mode(plt_build)    -> true;
is_plt_mode(plt_remove)   -> true;
is_plt_mode(plt_check)    -> true;
is_plt_mode(incremental)  -> true;
is_plt_mode(succ_typings) -> false.

assert_error_location(column) ->
  ok;
assert_error_location(line) ->
  ok;
assert_error_location(Term) ->
  bad_option("Illegal value for error_location", Term).

assert_solvers([]) ->
  ok;
assert_solvers([v1|Terms]) ->
  assert_solvers(Terms);
assert_solvers([v2|Terms]) ->
  assert_solvers(Terms);
assert_solvers([Term|_]) ->
  bad_option("Illegal value for solver", Term).

-spec build_warnings([atom()], dial_warn_tags()) -> dial_warn_tags().

%% The warning options are checked by the code linter.
%% The function erl_lint:is_module_dialyzer_option/1 must
%% be updated if options are added or removed.
build_warnings([Opt|Opts], Warnings) ->
  NewWarnings =
    case Opt of
      no_return ->
	ordsets:del_element(?WARN_RETURN_NO_RETURN, Warnings);
      no_unused ->
	ordsets:del_element(?WARN_NOT_CALLED, Warnings);
      no_unknown ->
    ordsets:del_element(?WARN_UNKNOWN, Warnings);
      no_improper_lists ->
	ordsets:del_element(?WARN_NON_PROPER_LIST, Warnings);
      no_fun_app ->
	ordsets:del_element(?WARN_FUN_APP, Warnings);
      no_match ->
	ordsets:del_element(?WARN_MATCHING, Warnings);
      no_opaque ->
        S = ordsets:from_list([?WARN_CONTRACT_OPAQUE,
                               ?WARN_OPAQUE,
                               ?WARN_OPAQUE_UNION]),
        ordsets:subtract(Warnings, S);
      no_fail_call ->
	ordsets:del_element(?WARN_FAILING_CALL, Warnings);
      no_contracts ->
        S = ordsets:from_list([?WARN_CONTRACT_OPAQUE,
                               ?WARN_CONTRACT_SYNTAX,
                               ?WARN_CONTRACT_TYPES,
                               ?WARN_OVERLAPPING_CONTRACT]),
        ordsets:subtract(Warnings, S);
      no_behaviours ->
	ordsets:del_element(?WARN_BEHAVIOUR, Warnings);
      no_undefined_callbacks ->
	ordsets:del_element(?WARN_UNDEFINED_CALLBACK, Warnings);
      unmatched_returns ->
	ordsets:add_element(?WARN_UNMATCHED_RETURN, Warnings);
      error_handling ->
	ordsets:add_element(?WARN_RETURN_ONLY_EXIT, Warnings);
      no_missing_calls ->
        ordsets:del_element(?WARN_CALLGRAPH, Warnings);
      specdiffs ->
        S = ordsets:from_list([?WARN_CONTRACT_SUBTYPE, 
                               ?WARN_CONTRACT_SUPERTYPE,
                               ?WARN_CONTRACT_NOT_EQUAL,
                               ?WARN_CONTRACT_MISSING_RETURN,
                               ?WARN_CONTRACT_EXTRA_RETURN]),
        ordsets:union(S, Warnings);
      overspecs ->
        S = ordsets:from_list([?WARN_CONTRACT_SUBTYPE,
                               ?WARN_CONTRACT_MISSING_RETURN]),
        ordsets:union(S, Warnings);
      underspecs ->
        S = ordsets:from_list([?WARN_CONTRACT_SUPERTYPE,
                               ?WARN_CONTRACT_EXTRA_RETURN]),
        ordsets:union(S, Warnings);
      no_underspecs ->
        ordsets:del_element(?WARN_CONTRACT_SUPERTYPE, Warnings);
      extra_return ->
        ordsets:add_element(?WARN_CONTRACT_EXTRA_RETURN, Warnings);
      no_extra_return ->
        ordsets:del_element(?WARN_CONTRACT_EXTRA_RETURN, Warnings);
      missing_return ->
        ordsets:add_element(?WARN_CONTRACT_MISSING_RETURN, Warnings);
      no_missing_return ->
        ordsets:del_element(?WARN_CONTRACT_MISSING_RETURN, Warnings);
      opaque_union ->
        ordsets:add_element(?WARN_OPAQUE_UNION, Warnings);
      no_opaque_union ->
        ordsets:del_element(?WARN_OPAQUE_UNION, Warnings);
      unknown ->
        ordsets:add_element(?WARN_UNKNOWN, Warnings);
      overlapping_contract ->
        ordsets:add_element(?WARN_OVERLAPPING_CONTRACT, Warnings);
      OtherAtom ->
        bad_option("Unknown dialyzer warning option", OtherAtom)
    end,
  build_warnings(Opts, NewWarnings);
build_warnings([], Warnings) ->
  Warnings.

%%-----------------------------------------------------------------------

%% Copied from compile.erl.
env_default_opts() ->
    Key = "ERL_COMPILER_OPTIONS",
    case os:getenv(Key) of
	false -> [];
	Str when is_list(Str) ->
	    case erl_scan:string(Str) of
		{ok,Tokens,_} ->
                    Dot = {dot, erl_anno:new(1)},
		    case erl_parse:parse_term(Tokens ++ [Dot]) of
			{ok,List} when is_list(List) -> List;
			{ok,Term} -> [Term];
			{error,_Reason} ->
			    io:format("Ignoring bad term in ~s\n", [Key]),
			    []
		    end;
		{error, {_,_,_Reason}, _} ->
		    io:format("Ignoring bad term in ~s\n", [Key]),
		    []
	    end
    end.
