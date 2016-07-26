%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2015. All Rights Reserved.
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
	io_lib:format("The PLT ~s includes the following files:\n~p\n\n",
		      [PLT, Files]);
      {error, read_error} ->
	Msg = io_lib:format("Could not read the PLT file ~p\n\n", [PLT]),
	throw({dialyzer_error, Msg});
      {error, no_such_file} ->
	Msg = io_lib:format("The PLT file ~p does not exist\n\n", [PLT]),
	throw({dialyzer_error, Msg})
    end,
  String ++ get_plt_info(PLTs);
get_plt_info([]) -> "".

do_print_plt_info(PLTInfo, OutputFile) ->
  case OutputFile =:= none of
    true ->
      io:format("~s", [PLTInfo]),
      ?RET_NOTHING_SUSPICIOUS;
    false ->
      case file:open(OutputFile, [write]) of
	{ok, FileDesc} ->
	  io:format(FileDesc, "~s", [PLTInfo]),
	  ok = file:close(FileDesc),
	  ?RET_NOTHING_SUSPICIOUS;
	{error, Reason} ->
	  Msg1 = io_lib:format("Could not open output file ~p, Reason: ~p\n",
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
  io:format("\ndialyzer: ~s\n", [Msg1]),
  cl_check_log(Output),
  halt(?RET_INTERNAL_ERROR).

-spec cl_check_log('none' | file:filename()) -> 'ok'.

cl_check_log(none) ->
  ok;
cl_check_log(Output) ->
  io:format("  Check output file `~s' for details\n", [Output]).

-spec format_warning(raw_warning() | dial_warning()) -> string().

format_warning(W) ->
  format_warning(W, basename).

-spec format_warning(raw_warning() | dial_warning(), fopt()) -> string().

format_warning({Tag, {File, Line, _MFA}, Msg}, FOpt) ->
  format_warning({Tag, {File, Line}, Msg}, FOpt);
format_warning({_Tag, {File, Line}, Msg}, FOpt) when is_list(File),
                                                     is_integer(Line) ->
  F = case FOpt of
	fullpath -> File;
	basename -> filename:basename(File)
      end,
  String = lists:flatten(message_to_string(Msg)),
  lists:flatten(io_lib:format("~s:~w: ~s", [F, Line, String])).


%%-----------------------------------------------------------------------------
%% Message classification and pretty-printing below. Messages appear in
%% categories and in more or less alphabetical ordering within each category.
%%-----------------------------------------------------------------------------

%%----- Warnings for general discrepancies ----------------
message_to_string({apply, [Args, ArgNs, FailReason,
			   SigArgs, SigRet, Contract]}) ->
  io_lib:format("Fun application with arguments ~s ", [Args]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({app_call, [M, F, Args, Culprit, ExpectedType, FoundType]}) ->
  io_lib:format("The call ~s:~s~s requires that ~s is of type ~s not ~s\n",
		[M, F, Args, Culprit, ExpectedType, FoundType]);
message_to_string({bin_construction, [Culprit, Size, Seg, Type]}) ->
  io_lib:format("Binary construction will fail since the ~s field ~s in"
		" segment ~s has type ~s\n", [Culprit, Size, Seg, Type]);
message_to_string({call, [M, F, Args, ArgNs, FailReason,
			  SigArgs, SigRet, Contract]}) ->
  io_lib:format("The call ~w:~w~s ", [M, F, Args]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({call_to_missing, [M, F, A]}) ->
  io_lib:format("Call to missing or unexported function ~w:~w/~w\n", [M, F, A]);
message_to_string({exact_eq, [Type1, Op, Type2]}) ->
  io_lib:format("The test ~s ~s ~s can never evaluate to 'true'\n",
		[Type1, Op, Type2]);
message_to_string({fun_app_args, [Args, Type]}) ->
  io_lib:format("Fun application with arguments ~s will fail"
		" since the function has type ~s\n", [Args, Type]);
message_to_string({fun_app_no_fun, [Op, Type, Arity]}) ->
  io_lib:format("Fun application will fail since ~s :: ~s"
		" is not a function of arity ~w\n", [Op, Type, Arity]);
message_to_string({guard_fail, []}) ->
  "Clause guard cannot succeed.\n";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}) ->
  io_lib:format("Guard test ~s ~s ~s can never succeed\n", [Arg1, Infix, Arg2]);
message_to_string({map_update, [Type, Key]}) ->
  io_lib:format("A key of type ~s cannot exist "
		"in a map of type ~s\n", [Key, Type]);
message_to_string({neg_guard_fail, [Arg1, Infix, Arg2]}) ->
  io_lib:format("Guard test not(~s ~s ~s) can never succeed\n",
		[Arg1, Infix, Arg2]);
message_to_string({guard_fail, [Guard, Args]}) ->
  io_lib:format("Guard test ~w~s can never succeed\n", [Guard, Args]);
message_to_string({neg_guard_fail, [Guard, Args]}) ->
  io_lib:format("Guard test not(~w~s) can never succeed\n", [Guard, Args]);
message_to_string({guard_fail_pat, [Pat, Type]}) ->
  io_lib:format("Clause guard cannot succeed. The ~s was matched"
		" against the type ~s\n", [Pat, Type]);
message_to_string({improper_list_constr, [TlType]}) ->
  io_lib:format("Cons will produce an improper list"
		" since its 2nd argument is ~s\n", [TlType]);
message_to_string({no_return, [Type|Name]}) ->
  NameString =
    case Name of
      [] -> "The created fun ";
      [F, A] -> io_lib:format("Function ~w/~w ", [F, A])
    end,
  case Type of
    no_match -> NameString ++ "has no clauses that will ever match\n";
    only_explicit -> NameString ++ "only terminates with explicit exception\n";
    only_normal -> NameString ++ "has no local return\n";
    both -> NameString ++ "has no local return\n"
  end;
message_to_string({record_constr, [RecConstr, FieldDiffs]}) ->
  io_lib:format("Record construction ~s violates the"
		" declared type of field ~s\n", [RecConstr, FieldDiffs]);
message_to_string({record_constr, [Name, Field, Type]}) ->
  io_lib:format("Record construction violates the declared type for #~w{}"
		" since ~s cannot be of type ~s\n", [Name, Field, Type]);
message_to_string({record_matching, [String, Name]}) ->
  io_lib:format("The ~s violates the"
		" declared type for #~w{}\n", [String, Name]);
message_to_string({record_match, [Pat, Type]}) ->
  io_lib:format("Matching of ~s tagged with a record name violates the declared"
		" type of ~s\n", [Pat, Type]);
message_to_string({pattern_match, [Pat, Type]}) ->
  io_lib:format("The ~s can never match the type ~s\n", [Pat, Type]);
message_to_string({pattern_match_cov, [Pat, Type]}) ->
  io_lib:format("The ~s can never match since previous"
		" clauses completely covered the type ~s\n",
		[Pat, Type]);
message_to_string({unmatched_return, [Type]}) ->
  io_lib:format("Expression produces a value of type ~s,"
		" but this value is unmatched\n", [Type]);
message_to_string({unused_fun, [F, A]}) ->
  io_lib:format("Function ~w/~w will never be called\n", [F, A]);
%%----- Warnings for specs and contracts -------------------
message_to_string({contract_diff, [M, F, _A, Contract, Sig]}) ->
  io_lib:format("Type specification ~w:~w~s"
		" is not equal to the success typing: ~w:~w~s\n",
		[M, F, Contract, M, F, Sig]);
message_to_string({contract_subtype, [M, F, _A, Contract, Sig]}) ->
  io_lib:format("Type specification ~w:~w~s"
		" is a subtype of the success typing: ~w:~w~s\n",
		[M, F, Contract, M, F, Sig]);
message_to_string({contract_supertype, [M, F, _A, Contract, Sig]}) ->
  io_lib:format("Type specification ~w:~w~s"
		" is a supertype of the success typing: ~w:~w~s\n",
		[M, F, Contract, M, F, Sig]);
message_to_string({contract_range, [Contract, M, F, ArgStrings, Line, CRet]}) ->
  io_lib:format("The contract ~w:~w~s cannot be right because the inferred"
		" return for ~w~s on line ~w is ~s\n",
		[M, F, Contract, F, ArgStrings, Line, CRet]);
message_to_string({invalid_contract, [M, F, A, Sig]}) ->
  io_lib:format("Invalid type specification for function ~w:~w/~w."
		" The success typing is ~s\n", [M, F, A, Sig]);
message_to_string({extra_range, [M, F, A, ExtraRanges, SigRange]}) ->
  io_lib:format("The specification for ~w:~w/~w states that the function"
		" might also return ~s but the inferred return is ~s\n",
		[M, F, A, ExtraRanges, SigRange]);
message_to_string({overlapping_contract, [M, F, A]}) ->
  io_lib:format("Overloaded contract for ~w:~w/~w has overlapping domains;"
		" such contracts are currently unsupported and are simply ignored\n",
		[M, F, A]);
message_to_string({spec_missing_fun, [M, F, A]}) ->
  io_lib:format("Contract for function that does not exist: ~w:~w/~w\n",
		[M, F, A]);
%%----- Warnings for opaque type violations -------------------
message_to_string({call_with_opaque, [M, F, Args, ArgNs, ExpArgs]}) ->
  io_lib:format("The call ~w:~w~s contains ~s when ~s\n",
		[M, F, Args, form_positions(ArgNs), form_expected(ExpArgs)]);
message_to_string({call_without_opaque, [M, F, Args, ExpectedTriples]}) ->
  io_lib:format("The call ~w:~w~s does not have ~s\n",
		[M, F, Args, form_expected_without_opaque(ExpectedTriples)]);
message_to_string({opaque_eq, [Type, _Op, OpaqueType]}) ->
  io_lib:format("Attempt to test for equality between a term of type ~s"
		" and a term of opaque type ~s\n", [Type, OpaqueType]);
message_to_string({opaque_guard, [Arg1, Infix, Arg2, ArgNs]}) ->
  io_lib:format("Guard test ~s ~s ~s contains ~s\n",
		[Arg1, Infix, Arg2, form_positions(ArgNs)]);
message_to_string({opaque_guard, [Guard, Args]}) ->
  io_lib:format("Guard test ~w~s breaks the opaqueness of its argument\n",
		[Guard, Args]);
message_to_string({opaque_match, [Pat, OpaqueType, OpaqueTerm]}) ->
  Term = if OpaqueType =:= OpaqueTerm -> "the term";
	    true -> OpaqueTerm
	 end,
  io_lib:format("The attempt to match a term of type ~s against the ~s"
		" breaks the opaqueness of ~s\n", [OpaqueType, Pat, Term]);
message_to_string({opaque_neq, [Type, _Op, OpaqueType]}) ->
  io_lib:format("Attempt to test for inequality between a term of type ~s"
		" and a term of opaque type ~s\n", [Type, OpaqueType]);
message_to_string({opaque_type_test, [Fun, Args, Arg, ArgType]}) ->
  io_lib:format("The type test ~s~s breaks the opaqueness of the term ~s~s\n",
                [Fun, Args, Arg, ArgType]);
message_to_string({opaque_size, [SizeType, Size]}) ->
  io_lib:format("The size ~s breaks the opaqueness of ~s\n",
                [SizeType, Size]);
message_to_string({opaque_call, [M, F, Args, Culprit, OpaqueType]}) ->
  io_lib:format("The call ~s:~s~s breaks the opaqueness of the term ~s :: ~s\n",
                [M, F, Args, Culprit, OpaqueType]);
%%----- Warnings for concurrency errors --------------------
message_to_string({race_condition, [M, F, Args, Reason]}) ->
  io_lib:format("The call ~w:~w~s ~s\n", [M, F, Args, Reason]);
%%----- Warnings for behaviour errors --------------------
message_to_string({callback_type_mismatch, [B, F, A, ST, CT]}) ->
  io_lib:format("The inferred return type of ~w/~w (~s) has nothing in common"
		" with ~s, which is the expected return type for the callback of"
		" ~w behaviour\n", [F, A, ST, CT, B]);
message_to_string({callback_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
  io_lib:format("The inferred type for the ~s argument of ~w/~w (~s) is"
		" not a supertype of ~s, which is expected type for this"
		" argument in the callback of the ~w behaviour\n",
		[ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_spec_type_mismatch, [B, F, A, ST, CT]}) ->
  io_lib:format("The return type ~s in the specification of ~w/~w is not a"
		" subtype of ~s, which is the expected return type for the"
		" callback of ~w behaviour\n", [ST, F, A, CT, B]);
message_to_string({callback_spec_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
  io_lib:format("The specified type for the ~s argument of ~w/~w (~s) is"
		" not a supertype of ~s, which is expected type for this"
		" argument in the callback of the ~w behaviour\n",
		[ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_missing, [B, F, A]}) ->
  io_lib:format("Undefined callback function ~w/~w (behaviour '~w')\n",
		[F, A, B]);
message_to_string({callback_info_missing, [B]}) ->
  io_lib:format("Callback info about the ~w behaviour is not available\n", [B]);
%%----- Warnings for unknown functions, types, and behaviours -------------
message_to_string({unknown_type, {M, F, A}}) ->
  io_lib:format("Unknown type ~w:~w/~w", [M, F, A]);
message_to_string({unknown_function, {M, F, A}}) ->
  io_lib:format("Unknown function ~w:~w/~w", [M, F, A]);
message_to_string({unknown_behaviour, B}) ->
  io_lib:format("Unknown behaviour ~w", [B]).

%%-----------------------------------------------------------------------------
%% Auxiliary functions below
%%-----------------------------------------------------------------------------

call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet,
			{IsOverloaded, Contract}) ->
  PositionString = form_position_string(ArgNs),
  case FailReason of
    only_sig ->
      case ArgNs =:= [] of
	true ->
	  %% We do not know which argument(s) caused the failure
	  io_lib:format("will never return since the success typing arguments"
			" are ~s\n", [SigArgs]);
        false ->
	  io_lib:format("will never return since it differs in the ~s argument"
			" from the success typing arguments: ~s\n",
			[PositionString, SigArgs])
      end;
    only_contract ->
      case (ArgNs =:= []) orelse IsOverloaded of
	true ->
	  %% We do not know which arguments caused the failure
	  io_lib:format("breaks the contract ~s\n", [Contract]);
	false ->
	  io_lib:format("breaks the contract ~s in the ~s argument\n",
			[Contract, PositionString])
      end;
    both ->
      io_lib:format("will never return since the success typing is ~s -> ~s"
		    " and the contract is ~s\n", [SigArgs, SigRet, Contract])
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
form_expected_without_opaque([{N, T, TStr}]) ->
  case erl_types:t_is_opaque(T) of
    true  ->
      io_lib:format("an opaque term of type ~s as ", [TStr]);
    false ->
      io_lib:format("a term of type ~s (with opaque subterms) as ", [TStr])
  end ++ form_position_string([N]) ++ " argument";
form_expected_without_opaque(ExpectedTriples) -> %% TODO: can do much better here
  {ArgNs, _Ts, _TStrs} = lists:unzip3(ExpectedTriples),
  "opaque terms as " ++ form_position_string(ArgNs) ++ " arguments".

form_expected(ExpectedArgs) ->
  case ExpectedArgs of
    [T] ->
      TS = erl_types:t_to_string(T),
      case erl_types:t_is_opaque(T) of
	true  -> io_lib:format("an opaque term of type ~s is expected", [TS]);
	false -> io_lib:format("a structured term of type ~s is expected", [TS])
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
