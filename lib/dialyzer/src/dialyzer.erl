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

%%%-------------------------------------------------------------------
%%% File        : dialyzer.erl
%%% Authors     : Tobias Lindahl <tobiasl@it.uu.se>
%%%               Kostis Sagonas <kostis@it.uu.se>
%%% Description : This is the interface for the Dialyzer tool.
%%%
%%% Created     : 27 Apr 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer).

%%--------------------------------------------------------------------
%% NOTE: Only functions exported by this module are available to
%%       other applications.
%%--------------------------------------------------------------------
-export([plain_cl/0,
	 run/1,
	 gui/0,
	 gui/1,
	 plt_info/1,
	 format_warning/1,
	 format_warning/2]).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------
%% Interfaces:
%%  - plain_cl/0 :      to be used ONLY by the dialyzer C program.
%%  - run/1:            Erlang interface for a command line-like analysis
%%  - gui/0/1:          Erlang interface for the gui.
%%  - format_warning/1: Get the string representation of a warning.
%%  - format_warning/1: Likewise, but with an option whether
%%			to display full path names or not
%%  - plt_info/1:       Get information of the specified plt.
%%--------------------------------------------------------------------

-spec plain_cl() -> no_return().

plain_cl() ->
  case dialyzer_cl_parse:start() of
    {check_init, Opts} ->
      cl_halt(cl_check_init(Opts), Opts);
    {plt_info, Opts} ->
      cl_halt(cl_print_plt_info(Opts), Opts);
    {gui, Opts} ->
      try check_gui_options(Opts)
      catch throw:{dialyzer_error, Msg} -> cl_error(Msg)
      end,
      case Opts#options.check_plt of
	true ->
	  case cl_check_init(Opts#options{get_warnings = false}) of
	    {ok, _} -> gui_halt(internal_gui(Opts), Opts);
	    {error, _} = Error -> cl_halt(Error, Opts)
	  end;
	false ->
	  gui_halt(internal_gui(Opts), Opts)
      end;
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
    case dialyzer_plt:included_files(PLT) of
      {ok, Files} ->
	io_lib:format("The PLT ~ts includes the following files:\n~tp\n\n",
		      [PLT, Files]);
      {error, read_error} ->
	Msg = io_lib:format("Could not read the PLT file ~tp\n\n", [PLT]),
	throw({dialyzer_error, Msg});
      {error, no_such_file} ->
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
  F = fun() ->
	  {Ret, _Warnings} = dialyzer_cl:start(Opts),
	  Ret
      end,
  doit(F).

-spec run(dial_options()) -> [dial_warning()].

run(Opts) ->
  try dialyzer_options:build([{report_mode, quiet},
			      {erlang_mode, true}|Opts]) of
    {error, Msg} ->
      throw({dialyzer_error, Msg});
    OptsRecord ->
      ok = check_init(OptsRecord),
      case dialyzer_cl:start(OptsRecord) of
        {?RET_DISCREPANCIES, Warnings} -> Warnings;
        {?RET_NOTHING_SUSPICIOUS, _}  -> []
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

internal_gui(OptsRecord) ->
  F = fun() ->
	  dialyzer_gui_wx:start(OptsRecord),
	  ?RET_NOTHING_SUSPICIOUS
      end,
  doit(F).

-spec gui() -> 'ok'.

gui() ->
  gui([]).

-spec gui(dial_options()) -> 'ok'.

gui(Opts) ->
  try dialyzer_options:build([{report_mode, quiet}|Opts]) of
    {error, Msg} ->
      throw({dialyzer_error, Msg});
    OptsRecord ->
      ok = check_gui_options(OptsRecord),
      ok = check_init(OptsRecord),
      F = fun() ->
          dialyzer_gui_wx:start(OptsRecord)
      end,
      case doit(F) of
	  {ok, _} -> ok;
	  {error, Msg} -> throw({dialyzer_error, Msg})
      end
  catch
    throw:{dialyzer_error, ErrorMsg} ->
      erlang:error({dialyzer_error, lists:flatten(ErrorMsg)})
  end.

check_gui_options(#options{analysis_type = succ_typings}) ->
  ok;
check_gui_options(#options{analysis_type = Mode}) ->
  Msg = io_lib:format("Analysis mode ~w is illegal in GUI mode", [Mode]),
  throw({dialyzer_error, Msg}).

-spec plt_info(file:filename()) ->
     {'ok', [{'files', [file:filename()]}]} | {'error', atom()}.

plt_info(Plt) ->
  case dialyzer_plt:included_files(Plt) of
    {ok, Files} -> {ok, [{files, Files}]};
    Error -> Error
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

-spec gui_halt(doit_ret(), #options{}) -> no_return().

gui_halt(R, Opts) ->
  cl_halt(R, Opts#options{report_mode = quiet}).

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

-spec format_warning(raw_warning() | dial_warning()) -> string().

format_warning(W) ->
  format_warning(W, basename).

-spec format_warning(raw_warning() | dial_warning(),
                     fopt() | proplists:proplist()) -> string().

format_warning(RawWarning, FOpt) when is_atom(FOpt) ->
  format_warning(RawWarning, [{filename_opt, FOpt}]);
format_warning({Tag, {File, Line, _MFA}, Msg}, Opts) ->
  format_warning({Tag, {File, Line}, Msg}, Opts);
format_warning({_Tag, {File, Line}, Msg}, Opts) when is_list(File),
                                                     is_integer(Line) ->
  F = case proplists:get_value(filename_opt, Opts, basename) of
	fullpath -> File;
	basename -> filename:basename(File)
      end,
  Indent = proplists:get_value(indent_opt, Opts, ?INDENT_OPT),
  String = message_to_string(Msg, Indent),
  lists:flatten(io_lib:format("~ts:~w: ~ts", [F, Line, String])).

%%-----------------------------------------------------------------------------
%% Message classification and pretty-printing below. Messages appear in
%% categories and in more or less alphabetical ordering within each category.
%%-----------------------------------------------------------------------------

%%----- Warnings for general discrepancies ----------------
message_to_string({apply, [Args, ArgNs, FailReason,
			   SigArgs, SigRet, Contract]}, I) ->
  io_lib:format("Fun application with arguments ~ts ", [a(Args, I)]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract, I);
message_to_string({app_call, [M, F, Args, Culprit, ExpectedType, FoundType]},
                  I) ->
  io_lib:format("The call ~s:~ts~ts requires that ~ts is of type ~ts not ~ts\n",
		[M, F, a(Args, I), c(Culprit, I),
                 t(ExpectedType, I), t(FoundType, I)]);
message_to_string({bin_construction, [Culprit, Size, Seg, Type]}, I) ->
  io_lib:format("Binary construction will fail since the ~s field ~s in"
		" segment ~s has type ~s\n",
                [Culprit, c(Size, I), c(Seg, I), t(Type, I)]);
message_to_string({call, [M, F, Args, ArgNs, FailReason,
			  SigArgs, SigRet, Contract]}, I) ->
  io_lib:format("The call ~w:~tw~ts ", [M, F, a(Args, I)]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract, I);
message_to_string({call_to_missing, [M, F, A]}, _I) ->
  io_lib:format("Call to missing or unexported function ~w:~tw/~w\n",
                [M, F, A]);
message_to_string({exact_eq, [Type1, Op, Type2]}, I) ->
  io_lib:format("The test ~ts ~s ~ts can never evaluate to 'true'\n",
		[t(Type1, I), Op, t(Type2, I)]);
message_to_string({fun_app_args, [ArgNs, Args, Type]}, I) ->
  PositionString = form_position_string(ArgNs),
  io_lib:format("Fun application with arguments ~ts will fail"
		" since the function has type ~ts,"
                " which differs in the ~s argument\n",
                [a(Args, I), t(Type, I), PositionString]);
message_to_string({fun_app_no_fun, [Op, Type, Arity]}, I) ->
  io_lib:format("Fun application will fail since ~ts :: ~ts"
		" is not a function of arity ~w\n", [Op, t(Type, I), Arity]);
message_to_string({guard_fail, []}, _I) ->
  "Clause guard cannot succeed.\n";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}, I) ->
  io_lib:format("Guard test ~ts ~s ~ts can never succeed\n",
                [a(Arg1, I), Infix, a(Arg2, I)]); % a/2 rather than c/2
message_to_string({map_update, [Type, Key]}, I) ->
  io_lib:format("A key of type ~ts cannot exist "
		"in a map of type ~ts\n", [t(Key, I), t(Type, I)]);
message_to_string({neg_guard_fail, [Arg1, Infix, Arg2]}, I) ->
  io_lib:format("Guard test not(~ts ~s ~ts) can never succeed\n",
		[a(Arg1, I), Infix, a(Arg2, I)]); % a/2 rather than c/2
message_to_string({guard_fail, [Guard, Args]}, I) ->
  io_lib:format("Guard test ~s~ts can never succeed\n", [Guard, a(Args, I)]);
message_to_string({neg_guard_fail, [Guard, Args]}, I) ->
  io_lib:format("Guard test not(~s~ts) can never succeed\n",
                [Guard, a(Args, I)]);
message_to_string({guard_fail_pat, [Pat, Type]}, I) ->
  io_lib:format("Clause guard cannot succeed. The ~ts was matched"
		" against the type ~ts\n", [ps(Pat, I), t(Type, I)]);
message_to_string({improper_list_constr, [TlType]}, I) ->
  io_lib:format("Cons will produce an improper list"
		" since its 2nd argument is ~ts\n", [t(TlType, I)]);
message_to_string({no_return, [Type|Name]}, _I) ->
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
message_to_string({record_constr, [RecConstr, FieldDiffs]}, I) ->
  io_lib:format("Record construction ~ts violates the"
		" declared type of field ~ts\n",
                [t(RecConstr, I), field_diffs(FieldDiffs, I)]);
message_to_string({record_constr, [Name, Field, Type]}, I) ->
  io_lib:format("Record construction violates the declared type for #~tw{}"
		" since ~ts cannot be of type ~ts\n",
                [Name, ps(Field, I), t(Type, I)]);
message_to_string({record_matching, [String, Name]}, I) ->
  io_lib:format("The ~ts violates the"
		" declared type for #~tw{}\n", [rec_type(String, I), Name]);
message_to_string({record_match, [Pat, Type]}, I) ->
  io_lib:format("Matching of ~ts tagged with a record name violates"
                " the declared type of ~ts\n", [ps(Pat, I), t(Type, I)]);
message_to_string({pattern_match, [Pat, Type]}, I) ->
  io_lib:format("The ~ts can never match the type ~ts\n",
                [ps(Pat, I), t(Type, I)]);
message_to_string({pattern_match_cov, [Pat, Type]}, I) ->
  io_lib:format("The ~ts can never match since previous"
		" clauses completely covered the type ~ts\n",
		[ps(Pat, I), t(Type, I)]);
message_to_string({unmatched_return, [Type]}, I) ->
  io_lib:format("Expression produces a value of type ~ts,"
		" but this value is unmatched\n", [t(Type, I)]);
message_to_string({unused_fun, [F, A]}, _I) ->
  io_lib:format("Function ~tw/~w will never be called\n", [F, A]);
%%----- Warnings for specs and contracts -------------------
message_to_string({contract_diff, [M, F, _A, Contract, Sig]}, I) ->
  io_lib:format("Type specification ~ts"
		" is not equal to the success typing: ~ts\n",
		[con(M, F, Contract, I), con(M, F, Sig, I)]);
message_to_string({contract_subtype, [M, F, _A, Contract, Sig]}, I) ->
  io_lib:format("Type specification ~ts"
		" is a subtype of the success typing: ~ts\n",
		[con(M, F, Contract, I), con(M, F, Sig, I)]);
message_to_string({contract_supertype, [M, F, _A, Contract, Sig]}, I) ->
  io_lib:format("Type specification ~ts"
		" is a supertype of the success typing: ~ts\n",
		[con(M, F, Contract, I), con(M, F, Sig, I)]);
message_to_string({contract_range, [Contract, M, F, ArgStrings, Line, CRet]},
                 I) ->
  io_lib:format("The contract ~ts cannot be right because the inferred"
		" return for ~tw~ts on line ~w is ~ts\n",
		[con(M, F, Contract, I), F, a(ArgStrings, I), Line, t(CRet, I)]);
message_to_string({invalid_contract, [M, F, A, Sig]}, I) ->
  io_lib:format("Invalid type specification for function ~w:~tw/~w."
		" The success typing is ~ts\n", [M, F, A, sig(Sig, I)]);
message_to_string({contract_with_opaque, [M, F, A, OpaqueType, SigType]},
                 I) ->
  io_lib:format("The specification for ~w:~tw/~w"
                " has an opaque subtype ~ts which is violated by the"
                " success typing ~ts\n",
                [M, F, A, t(OpaqueType, I), sig(SigType, I)]);
message_to_string({extra_range, [M, F, A, ExtraRanges, SigRange]}, I) ->
  io_lib:format("The specification for ~w:~tw/~w states that the function"
		" might also return ~ts but the inferred return is ~ts\n",
		[M, F, A, t(ExtraRanges, I), t(SigRange, I)]);
message_to_string({missing_range, [M, F, A, ExtraRanges, ContrRange]}, I) ->
  io_lib:format("The success typing for ~w:~tw/~w implies that the function"
		" might also return ~ts but the specification return is ~ts\n",
		[M, F, A, t(ExtraRanges, I), t(ContrRange, I)]);
message_to_string({overlapping_contract, [M, F, A]}, _I) ->
  io_lib:format("Overloaded contract for ~w:~tw/~w has overlapping domains;"
		" such contracts are currently unsupported and are simply ignored\n",
		[M, F, A]);
message_to_string({spec_missing_fun, [M, F, A]}, _I) ->
  io_lib:format("Contract for function that does not exist: ~w:~tw/~w\n",
		[M, F, A]);
%%----- Warnings for opaque type violations -------------------
message_to_string({call_with_opaque, [M, F, Args, ArgNs, ExpArgs]}, I) ->
  io_lib:format("The call ~w:~tw~ts contains ~ts when ~ts\n",
		[M, F, a(Args, I), form_positions(ArgNs),
                 form_expected(ExpArgs, I)]);
message_to_string({call_without_opaque, [M, F, Args, ExpectedTriples]}, I) ->
  io_lib:format("The call ~w:~tw~ts does not have ~ts\n",
		[M, F, a(Args, I),
                 form_expected_without_opaque(ExpectedTriples, I)]);
message_to_string({opaque_eq, [Type, _Op, OpaqueType]}, I) ->
  io_lib:format("Attempt to test for equality between a term of type ~ts"
		" and a term of opaque type ~ts\n",
                [t(Type, I), t(OpaqueType, I)]);
message_to_string({opaque_guard, [Arg1, Infix, Arg2, ArgNs]}, I) ->
  io_lib:format("Guard test ~ts ~s ~ts contains ~s\n",
		[a(Arg1, I), Infix, a(Arg2, I), form_positions(ArgNs)]);
message_to_string({opaque_guard, [Guard, Args]}, I) ->
  io_lib:format("Guard test ~w~ts breaks the opacity of its argument\n",
		[Guard, a(Args, I)]);
message_to_string({opaque_match, [Pat, OpaqueType, OpaqueTerm]}, I) ->
  Term = if OpaqueType =:= OpaqueTerm -> "the term";
	    true -> t(OpaqueTerm, I)
	 end,
  io_lib:format("The attempt to match a term of type ~ts against the ~ts"
		" breaks the opacity of ~ts\n",
                [t(OpaqueType, I), ps(Pat, I), Term]);
message_to_string({opaque_neq, [Type, _Op, OpaqueType]}, I) ->
  io_lib:format("Attempt to test for inequality between a term of type ~ts"
		" and a term of opaque type ~ts\n",
                [t(Type, I), t(OpaqueType, I)]);
message_to_string({opaque_type_test, [Fun, Args, Arg, ArgType]}, I) ->
  io_lib:format("The type test ~ts~ts breaks the opacity of the term ~ts~ts\n",
                [Fun, a(Args, I), Arg, t(ArgType, I)]);
message_to_string({opaque_size, [SizeType, Size]}, I) ->
  io_lib:format("The size ~ts breaks the opacity of ~ts\n",
                [t(SizeType, I), c(Size, I)]);
message_to_string({opaque_call, [M, F, Args, Culprit, OpaqueType]}, I) ->
  io_lib:format("The call ~s:~ts~ts breaks the opacity of the term ~ts :: ~ts\n",
                [M, F, a(Args, I), c(Culprit, I), t(OpaqueType, I)]);
%%----- Warnings for concurrency errors --------------------
message_to_string({race_condition, [M, F, Args, Reason]}, I) ->
  %% There is a possibly huge type in Reason.
  io_lib:format("The call ~w:~tw~ts ~ts\n", [M, F, a(Args, I), Reason]);
%%----- Warnings for behaviour errors --------------------
message_to_string({callback_type_mismatch, [B, F, A, ST, CT]}, I) ->
  io_lib:format("The inferred return type of ~tw/~w ~ts has nothing in"
                " common with ~ts, which is the expected return type for"
                " the callback of the ~w behaviour\n",
                [F, A, t("("++ST++")", I), t(CT, I), B]);
message_to_string({callback_arg_type_mismatch, [B, F, A, N, ST, CT]}, I) ->
  io_lib:format("The inferred type for the ~s argument of ~tw/~w (~ts) is"
		" not a supertype of ~ts, which is expected type for this"
		" argument in the callback of the ~w behaviour\n",
		[ordinal(N), F, A, t(ST, I), t(CT, I), B]);
message_to_string({callback_spec_type_mismatch, [B, F, A, ST, CT]}, I) ->
  io_lib:format("The return type ~ts in the specification of ~tw/~w is not a"
		" subtype of ~ts, which is the expected return type for the"
		" callback of the ~w behaviour\n",
                [t(ST, I), F, A, t(CT, I), B]);
message_to_string({callback_spec_arg_type_mismatch, [B, F, A, N, ST, CT]},
                  I) ->
  io_lib:format("The specified type for the ~ts argument of ~tw/~w (~ts) is"
		" not a supertype of ~ts, which is expected type for this"
		" argument in the callback of the ~w behaviour\n",
		[ordinal(N), F, A, t(ST, I), t(CT, I), B]);
message_to_string({callback_missing, [B, F, A]}, _I) ->
  io_lib:format("Undefined callback function ~tw/~w (behaviour ~w)\n",
		[F, A, B]);
message_to_string({callback_info_missing, [B]}, _I) ->
  io_lib:format("Callback info about the ~w behaviour is not available\n", [B]);
%%----- Warnings for unknown functions, types, and behaviours -------------
message_to_string({unknown_type, {M, F, A}}, _I) ->
  io_lib:format("Unknown type ~w:~tw/~w", [M, F, A]);
message_to_string({unknown_function, {M, F, A}}, _I) ->
  io_lib:format("Unknown function ~w:~tw/~w", [M, F, A]);
message_to_string({unknown_behaviour, B}, _I) ->
  io_lib:format("Unknown behaviour ~w", [B]).

%%-----------------------------------------------------------------------------
%% Auxiliary functions below
%%-----------------------------------------------------------------------------

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
 end ++ form_position_string(ArgNs) ++
  case ArgNs of
    [_] -> " argument";
    [_,_|_] -> " arguments"
  end.

%% We know which positions N are to blame;
%% the list of triples will never be empty.
form_expected_without_opaque([{N, T, TStr}], I) ->
  case erl_types:t_is_opaque(T) of
    true  ->
      io_lib:format("an opaque term of type ~ts as ", [t(TStr, I)]);
    false ->
      io_lib:format("a term of type ~ts (with opaque subterms) as ",
                    [t(TStr, I)])
  end ++ form_position_string([N]) ++ " argument";
form_expected_without_opaque(ExpectedTriples, _I) -> %% TODO: can do much better here
  {ArgNs, _Ts, _TStrs} = lists:unzip3(ExpectedTriples),
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

ordinal(1) -> "1st";
ordinal(2) -> "2nd";
ordinal(3) -> "3rd";
ordinal(N) when is_integer(N) -> io_lib:format("~wth", [N]).

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
                        [{capture, all_but_first, list}, dotall]),
  S.

pp_spec(Spec) ->
  Form = {attribute, erl_anno:new(0), spec, {{a,b,0}, Spec}},
  Sig = erl_pp:form(Form, [{quote_singleton_atom_types, true}]),
  {match, [S]} = re:run(Sig, <<"-spec a:b\\s*(.*)\\.\\n*">>,
                        [{capture, all_but_first, list}, dotall]),
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
