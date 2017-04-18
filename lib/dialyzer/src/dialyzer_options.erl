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
%%
%% @copyright 2004 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @doc Provides a better way to start Dialyzer from a script.

-module(dialyzer_options).

-export([build/1, build_warnings/2]).

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------

-spec build(dial_options()) -> #options{} | {'error', string()}.

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
		  ?WARN_CONTRACT_RANGE,
		  ?WARN_CONTRACT_TYPES,
		  ?WARN_CONTRACT_SYNTAX,
		  ?WARN_BEHAVIOUR,
		  ?WARN_UNDEFINED_CALLBACK],
  DefaultWarns1 = ordsets:from_list(DefaultWarns),
  InitPlt = dialyzer_plt:get_default_plt(),
  DefaultOpts = #options{},
  DefaultOpts1 = DefaultOpts#options{legal_warnings = DefaultWarns1,
                                     init_plts = [InitPlt]},
  try
    Opts1 = preprocess_opts(Opts),
    NewOpts = build_options(Opts1, DefaultOpts1),
    postprocess_opts(NewOpts)
  catch
    throw:{dialyzer_options_error, Msg} -> {error, Msg}
  end.

preprocess_opts([]) -> [];
preprocess_opts([{init_plt, File}|Opts]) ->
  [{plts, [File]}|preprocess_opts(Opts)];
preprocess_opts([Opt|Opts]) ->
  [Opt|preprocess_opts(Opts)].

postprocess_opts(Opts = #options{}) ->
  check_file_existence(Opts),
  Opts1 = check_output_plt(Opts),
  adapt_get_warnings(Opts1).

check_file_existence(#options{analysis_type = plt_remove}) -> ok;
check_file_existence(#options{files = Files, files_rec = FilesRec}) ->
  assert_filenames_exist(Files),
  assert_filenames_exist(FilesRec).

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

adapt_get_warnings(Opts = #options{analysis_type = Mode,
				   get_warnings = Warns}) ->
  %% Warnings are off by default in plt mode, and on by default in
  %% success typings mode. User defined warning mode overrides the
  %% default.
  case is_plt_mode(Mode) of
    true ->
      case Warns =:= maybe of
	true -> Opts#options{get_warnings = false};
	false -> Opts
      end;
    false ->
      case Warns =:= maybe of
	true -> Opts#options{get_warnings = true};
	false -> Opts
      end
  end.

-spec bad_option(string(), term()) -> no_return().

bad_option(String, Term) ->
  Msg = io_lib:format("~s: ~P", [String, Term, 25]),
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
    analysis_type ->
      NewOptions =
	case Value of
	  succ_typings -> Options#options{analysis_type = Value};
	  plt_add      -> Options#options{analysis_type = Value};
	  plt_build    -> Options#options{analysis_type = Value};
	  plt_check    -> Options#options{analysis_type = Value};
	  plt_remove   -> Options#options{analysis_type = Value};
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
      assert_filenames(Term, Value),
      build_options(Rest, Options#options{init_plts = Value});
    include_dirs ->
      assert_filenames(Term, Value),
      OldVal = Options#options.include_dirs,
      NewVal = ordsets:union(ordsets:from_list(Value), OldVal),
      build_options(Rest, Options#options{include_dirs = NewVal});
    use_spec ->
      build_options(Rest, Options#options{use_contracts = Value});
    old_style ->
      bad_option("Analysis type is no longer supported", old_style);
    output_file ->
      assert_filename(Value),
      build_options(Rest, Options#options{output_file = Value});
    output_format ->
      assert_output_format(Value),
      build_options(Rest, Options#options{output_format = Value});
    filename_opt ->
      assert_filename_opt(Value),
      build_options(Rest, Options#options{filename_opt = Value});
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
    timing ->
      build_options(Rest, Options#options{timing = Value});
    solvers ->
      assert_solvers(Value),
      build_options(Rest, Options#options{solvers = Value});
    _ ->
      bad_option("Unknown dialyzer command line option", Term)
  end;
build_options([], Options) ->
  Options.

get_app_dirs(Apps) when is_list(Apps) ->
  dialyzer_cl_parse:get_lib_dir([atom_to_list(A) || A <- Apps]);
get_app_dirs(Apps) ->
  bad_option("Use a list of otp applications", Apps).

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
is_plt_mode(succ_typings) -> false.

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
      no_improper_lists ->
	ordsets:del_element(?WARN_NON_PROPER_LIST, Warnings);
      no_fun_app ->
	ordsets:del_element(?WARN_FUN_APP, Warnings);
      no_match ->
	ordsets:del_element(?WARN_MATCHING, Warnings);
      no_opaque ->
	ordsets:del_element(?WARN_OPAQUE, Warnings);
      no_fail_call ->
	ordsets:del_element(?WARN_FAILING_CALL, Warnings);
      no_contracts ->
	Warnings1 = ordsets:del_element(?WARN_CONTRACT_SYNTAX, Warnings),
	ordsets:del_element(?WARN_CONTRACT_TYPES, Warnings1);
      no_behaviours ->
	ordsets:del_element(?WARN_BEHAVIOUR, Warnings);
      no_undefined_callbacks ->
	ordsets:del_element(?WARN_UNDEFINED_CALLBACK, Warnings);
      unmatched_returns ->
	ordsets:add_element(?WARN_UNMATCHED_RETURN, Warnings);
      error_handling ->
	ordsets:add_element(?WARN_RETURN_ONLY_EXIT, Warnings);
      race_conditions ->
	ordsets:add_element(?WARN_RACE_CONDITION, Warnings);
      no_missing_calls ->
        ordsets:del_element(?WARN_CALLGRAPH, Warnings);
      specdiffs ->
	S = ordsets:from_list([?WARN_CONTRACT_SUBTYPE, 
			       ?WARN_CONTRACT_SUPERTYPE,
			       ?WARN_CONTRACT_NOT_EQUAL]),
	ordsets:union(S, Warnings);
      overspecs ->
	ordsets:add_element(?WARN_CONTRACT_SUBTYPE, Warnings);
      underspecs ->
	ordsets:add_element(?WARN_CONTRACT_SUPERTYPE, Warnings);
      unknown ->
	ordsets:add_element(?WARN_UNKNOWN, Warnings);
      OtherAtom ->
	bad_option("Unknown dialyzer warning option", OtherAtom)
    end,
  build_warnings(Opts, NewWarnings);
build_warnings([], Warnings) ->
  Warnings.
