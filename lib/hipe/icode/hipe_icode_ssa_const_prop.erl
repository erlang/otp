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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ============================================================================
%%  Filename :  hipe_icode_ssa_const_prop.erl
%%  Authors  :  Daniel Luna, Erik Andersson
%%  Purpose  :  Perform sparse conditional constant propagation on Icode.
%%  Notes    :  Works on the control-flow graph.
%%
%%  History  : * 2003-03-05: Created.
%%             * 2003-08-11: Passed simple testsuite.
%%             * 2003-10-01: Passed compiler testsuite.
%% ============================================================================
%%
%% Exports: propagate/1.
%%
%% ============================================================================
%%
%% TODO: 
%%
%% Take care of failures in call and replace operation with appropriate
%% failure.
%%
%% Handle ifs with non-binary operators
%%
%% We want multisets for easier (and faster) creation of env->ssa_edges
%% 
%% Maybe do things with begin_handler, begin_try if possible
%%
%% Propagation of constant arguments when some of the arguments are bottom
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_ssa_const_prop).
-export([propagate/1]).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").
-include("hipe_icode_primops.hrl").

-define(CONST_PROP_MSG(Str,L), ok).
%%-define(CONST_PROP_MSG(Str,L), io:format(Str,L)).

%%-define(DEBUG, 1).

%%-----------------------------------------------------------------------------
%% Include stuff shared between SCCP on Icode and RTL.
%% NOTE: Needs to appear after DEBUG is possibly defined.
%%-----------------------------------------------------------------------------

-define(CODE, hipe_icode).
-define(CFG,  hipe_icode_cfg).

-include("../ssa/hipe_ssa_const_prop.inc").

%%-----------------------------------------------------------------------------

visit_expression(Instruction, Environment) ->
  EvaluatedArguments = [lookup_lattice_value(Argument, Environment) 
			|| Argument <- hipe_icode:args(Instruction)],
  case Instruction of
    #icode_move{}  ->
      visit_move              (Instruction, EvaluatedArguments, Environment);
    #icode_if{} ->
      visit_if                (Instruction, EvaluatedArguments, Environment);
    #icode_goto{} ->
      visit_goto              (Instruction, EvaluatedArguments, Environment);
    #icode_type{} ->
      visit_type              (Instruction, EvaluatedArguments, Environment);
    #icode_call{} ->
      visit_call              (Instruction, EvaluatedArguments, Environment);
    #icode_switch_val{} ->
      visit_switch_val        (Instruction, EvaluatedArguments, Environment);
    #icode_switch_tuple_arity{} ->
      visit_switch_tuple_arity(Instruction, EvaluatedArguments, Environment);
    #icode_begin_handler{} ->
      visit_begin_handler     (Instruction, EvaluatedArguments, Environment);
    #icode_begin_try{} ->
      visit_begin_try         (Instruction, EvaluatedArguments, Environment);
    #icode_fail{} ->
      visit_fail              (Instruction, EvaluatedArguments, Environment);
    #icode_comment{} -> {[], [], Environment};
    #icode_end_try{} -> {[], [], Environment};
    #icode_enter{} ->   {[], [], Environment};
    #icode_label{} ->   {[], [], Environment};
    #icode_return{} ->  {[], [], Environment}
  end.

%%-----------------------------------------------------------------------------

visit_begin_try(Instruction, [], Environment) ->
  Label     = hipe_icode:begin_try_label(Instruction), 
  Successor = hipe_icode:begin_try_successor(Instruction),
  {[Label, Successor], [], Environment}.

%%-----------------------------------------------------------------------------

visit_begin_handler(Instruction, _Arguments, Environment) ->
  Destinations = hipe_icode:begin_handler_dstlist(Instruction),
  {Environment1, SSAWork} =
    lists:foldl(fun (Dst, {Env0,Work0}) ->
		    {Env, Work} = update_lattice_value({Dst, bottom}, Env0),
		    {Env, Work ++ Work0}
		end,
		{Environment, []},
		Destinations),
  {[], SSAWork, Environment1}.

%%-----------------------------------------------------------------------------

visit_switch_val(Instruction, [Argument], Environment) ->
  Cases     = hipe_icode:switch_val_cases(Instruction),
  FailLabel = hipe_icode:switch_val_fail_label(Instruction),
  case Argument of
    bottom ->
      FlowWork  = [Label || {_Value, Label} <- Cases],
      FlowWork1 = [FailLabel | FlowWork],
      {FlowWork1, [], Environment};
    _ ->
      Target = get_switch_target(Cases, Argument, FailLabel),
      {[Target], [], Environment}
  end.

%%-----------------------------------------------------------------------------

visit_switch_tuple_arity(Instruction, [Argument], Environment) ->
  Cases     = hipe_icode:switch_tuple_arity_cases(Instruction),
  FailLabel = hipe_icode:switch_tuple_arity_fail_label(Instruction),
  case Argument of
    bottom ->
      FlowWork  = [Label || {_Value, Label} <- Cases],
      FlowWork1 = [FailLabel | FlowWork],
      {FlowWork1, [], Environment};
    Constant ->
      UnTagged = hipe_icode:const_value(Constant),
      case is_tuple(UnTagged) of
	true ->
	  Target = get_switch_target(Cases, tuple_size(UnTagged), FailLabel),
	  {[Target], [], Environment};
	false ->
	  {[FailLabel], [], Environment}
      end
  end.

%%-----------------------------------------------------------------------------

get_switch_target([], _Argument, FailLabel) ->
  FailLabel;
get_switch_target([{CaseValue, Target} | CaseList], Argument, FailLabel) ->
  case CaseValue =:= Argument of
    true ->
      Target;
    false ->
      get_switch_target(CaseList, Argument, FailLabel)
  end.

%%-----------------------------------------------------------------------------

visit_move(Instruction, [SourceValue], Environment) ->
  Destination = hipe_icode:move_dst(Instruction),
  {Environment1, SSAWork} = update_lattice_value({Destination, SourceValue},
						 Environment),
  {[], SSAWork, Environment1}.

%%-----------------------------------------------------------------------------

visit_if(Instruction, Arguments, Environment) ->
  FlowWork =
    case evaluate_if(hipe_icode:if_op(Instruction), Arguments) of
      true ->
	TrueLabel  = hipe_icode:if_true_label(Instruction),
	[TrueLabel];
      false ->
	FalseLabel = hipe_icode:if_false_label(Instruction),
	[FalseLabel];
      bottom ->
	TrueLabel  = hipe_icode:if_true_label(Instruction),
	FalseLabel = hipe_icode:if_false_label(Instruction),
	[TrueLabel, FalseLabel]
    end,
  {FlowWork, [], Environment}.

%%-----------------------------------------------------------------------------

visit_goto(Instruction, _Arguments, Environment) ->
  GotoLabel = hipe_icode:goto_label(Instruction),
  FlowWork  = [GotoLabel],
  {FlowWork, [], Environment}.
              
%%-----------------------------------------------------------------------------

visit_fail(Instruction, _Arguments, Environment) ->
  FlowWork = hipe_icode:successors(Instruction),
  {FlowWork, [], Environment}.

%%-----------------------------------------------------------------------------

visit_type(Instruction, Values, Environment) ->
  FlowWork =
    case evaluate_type(hipe_icode:type_test(Instruction), Values) of
      true ->
	TrueLabel  = hipe_icode:type_true_label(Instruction),
	[TrueLabel];
    false ->
	FalseLabel = hipe_icode:type_false_label(Instruction),
	[FalseLabel];
      bottom ->
	TrueLabel  = hipe_icode:type_true_label(Instruction),
	FalseLabel = hipe_icode:type_false_label(Instruction),
	[TrueLabel, FalseLabel]
    end,
  {FlowWork, [], Environment}.

%%-----------------------------------------------------------------------------

visit_call(Ins, Args, Environment) ->
  Dsts = hipe_icode:call_dstlist(Ins),
  Fun = hipe_icode:call_fun(Ins),
  Fail = call_fail_labels(Ins),
  Cont = call_continuation_labels(Ins),
  visit_call(Dsts, Args, Fun, Cont, Fail, Environment).

visit_call(Dst, Args, Fun, Cont, Fail, Environment) ->
  {FlowWork, {Environment1, SSAWork}} =
    case lists:any(fun(X) -> (X =:= bottom) end, Args) of
      true ->
	{Fail ++ Cont, update_lattice_value({Dst, bottom}, Environment)};
      false ->
	ConstArgs = [hipe_icode:const_value(Argument) || Argument <- Args],
	try evaluate_call_or_enter(ConstArgs, Fun) of
	  bottom ->
	    {Fail ++ Cont, update_lattice_value({Dst, bottom}, Environment)};
	  Constant ->
	    {Cont, update_lattice_value({Dst, Constant}, Environment)}
	catch 
	  _:_ ->
	    {Fail, update_lattice_value({Dst, bottom}, Environment)}
	end
    end,
  {FlowWork, SSAWork, Environment1}.

%%-----------------------------------------------------------------------------

call_fail_labels(I) ->
  case hipe_icode:call_fail_label(I) of
    [] -> [];
    Label -> [Label]
  end.

call_continuation_labels(I) ->
  case hipe_icode:call_continuation(I) of
    [] -> [];
    Label -> [Label]
  end.
  
%%-----------------------------------------------------------------------------

%% Unary calls
evaluate_call_or_enter([Argument], Fun) ->
  case Fun of
    mktuple ->
      hipe_icode:mk_const(list_to_tuple([Argument]));
    unsafe_untag_float ->
      hipe_icode:mk_const(float(Argument));
    conv_to_float ->
      hipe_icode:mk_const(float(Argument));
    fnegate ->
      hipe_icode:mk_const(0.0 - Argument);
    'bnot' ->
      hipe_icode:mk_const(Argument);
    #unsafe_element{index=N} ->
      hipe_icode:mk_const(element(N, Argument));
    {erlang, hd, 1} ->
      hipe_icode:mk_const(hd(Argument));
    {erlang, tl, 1} ->
      hipe_icode:mk_const(tl(Argument));
    {erlang, atom_to_list, 1} ->
      hipe_icode:mk_const(atom_to_list(Argument));
    {erlang, list_to_atom, 1} ->
      hipe_icode:mk_const(list_to_atom(Argument));
    {erlang, tuple_to_list, 1} ->
      hipe_icode:mk_const(tuple_to_list(Argument));
    {erlang, list_to_tuple, 1} ->
      hipe_icode:mk_const(list_to_tuple(Argument));
    {erlang, length, 1} ->
      hipe_icode:mk_const(length(Argument));
    {erlang, size, 1} ->
      hipe_icode:mk_const(size(Argument));
    {erlang, bit_size, 1} ->
      hipe_icode:mk_const(bit_size(Argument));
    {erlang, byte_size, 1} ->
      hipe_icode:mk_const(byte_size(Argument));
    {erlang, tuple_size, 1} ->
      hipe_icode:mk_const(tuple_size(Argument));
    {erlang, abs, 1} ->
      hipe_icode:mk_const(abs(Argument));
    {erlang, round, 1} ->
      hipe_icode:mk_const(round(Argument));
    {erlang, trunc, 1} ->
      hipe_icode:mk_const(trunc(Argument));
    _ ->
      bottom
  end;
%% Binary calls
evaluate_call_or_enter([Argument1,Argument2], Fun) ->
  case Fun of
    '+' ->
      hipe_icode:mk_const(Argument1 + Argument2);
    '-' ->
      hipe_icode:mk_const(Argument1 - Argument2);
    '*' ->
      hipe_icode:mk_const(Argument1 * Argument2);
    '/' ->
      hipe_icode:mk_const(Argument1 / Argument2);
    'band' ->
      hipe_icode:mk_const(Argument1 band Argument2);
    'bor' ->
      hipe_icode:mk_const(Argument1 bor Argument2);
    'bsl' ->
      hipe_icode:mk_const(Argument1 bsl Argument2);
    'bsr' ->
      hipe_icode:mk_const(Argument1 bsr Argument2);
    'bxor' ->
      hipe_icode:mk_const(Argument1 bxor Argument2);
    fp_add ->
      hipe_icode:mk_const(float(Argument1 + Argument2));
    fp_sub ->
      hipe_icode:mk_const(float(Argument1 - Argument2));
    fp_mul ->
      hipe_icode:mk_const(float(Argument1 * Argument2));
    fp_div ->
      hipe_icode:mk_const(Argument1 / Argument2);
    cons ->
      hipe_icode:mk_const([Argument1 | Argument2]);
    mktuple -> 
      hipe_icode:mk_const(list_to_tuple([Argument1,Argument2]));
    #unsafe_update_element{index=N} ->
      hipe_icode:mk_const(setelement(N, Argument1, Argument2));
    {erlang, '++', 2} ->
      hipe_icode:mk_const(Argument1 ++ Argument2);
    {erlang, '--', 2} ->
      hipe_icode:mk_const(Argument1 -- Argument2);
    {erlang, 'div', 2} ->
      hipe_icode:mk_const(Argument1 div Argument2);
    {erlang, 'rem', 2} ->
      hipe_icode:mk_const(Argument1 rem Argument2);
    {erlang, append_element, 2} ->
      hipe_icode:mk_const(erlang:append_element(Argument1, Argument2));
    {erlang, element, 2} ->
      hipe_icode:mk_const(element(Argument1, Argument2));
    _Other ->
      %% io:format("In ~w(~w,~w)~n", [_Other,Argument1,Argument2]),
      bottom
  end;

%% The rest of the calls
evaluate_call_or_enter(Arguments, Fun) ->
  case Fun of
    mktuple ->
      hipe_icode:mk_const(list_to_tuple(Arguments));
    {erlang, setelement, 3} ->
      [Argument1, Argument2, Argument3] = Arguments,
      hipe_icode:mk_const(setelement(Argument1, Argument2, Argument3));
    _ ->
      bottom
  end.

%%-----------------------------------------------------------------------------

evaluate_if(Conditional, [Argument1, Argument2]) -> 
  case ((Argument1 =:= bottom) or (Argument2 =:= bottom)) of
    true  -> bottom;
    false -> evaluate_if_const(Conditional, Argument1, Argument2)
  end;
evaluate_if(_Conditional, _Arguments) ->
  bottom.

%%-----------------------------------------------------------------------------

evaluate_if_const(Conditional, Argument1, Argument2) ->
  case Conditional of
    '=:=' -> Argument1 =:= Argument2;
    '=='  -> Argument1 ==  Argument2;
    '=/=' -> Argument1 =/= Argument2;
    '/='  -> Argument1  /= Argument2;
    '<'   -> Argument1  <  Argument2;
    '>='  -> Argument1  >= Argument2;
    '=<'  -> Argument1 =<  Argument2;
    '>'   -> Argument1  >  Argument2;
    _     -> bottom
  end.

%%-----------------------------------------------------------------------------

evaluate_type(Type, Vals) ->
  case [X || X <- Vals, X =:= bottom] of
    [] -> evaluate_type_const(Type, Vals);
    _ -> bottom
  end.

%%-----------------------------------------------------------------------------

evaluate_type_const(Type, [Arg|Left]) ->
  Test = 
    case {Type, hipe_icode:const_value(Arg)} of
      {nil,    []   }  -> true;
      {nil,    _    }  -> false;
      {cons,   [_|_]}  -> true;
      {cons,   _    }  -> false;
      {{tuple, N}, T} when tuple_size(T) =:= N -> true;
      {atom,       A} when is_atom(A) -> true;
      {{atom, A},  A} when is_atom(A) -> true;
      {{record, A, S}, R} when tuple_size(R) =:= S, 
			       element(1, R) =:= A -> true;
      {{record, _, _}, _} -> false;
      _                -> bottom
    end,
  case Test of
    bottom -> bottom;
    false -> false;
    true -> evaluate_type_const(Type, Left)
  end;
evaluate_type_const(_Type, []) ->
  true.

%%-----------------------------------------------------------------------------
%% Icode-specific code below
%%-----------------------------------------------------------------------------

update_instruction(Instruction, Environment) ->
  case Instruction of
    #icode_call{} ->
      update_call(Instruction, Environment);
    #icode_enter{} ->
      update_enter(Instruction, Environment);
    #icode_if{} ->
      update_if(Instruction, Environment);
    #icode_move{} ->
      update_move(Instruction, Environment);
    #icode_phi{} ->
     update_phi(Instruction, Environment);
    #icode_switch_val{} ->
      update_switch_val(Instruction, Environment);
    #icode_type{} ->
      update_type(Instruction, Environment);
    #icode_switch_tuple_arity{} ->
      update_switch_tuple_arity(Instruction, Environment);
    %% We could but don't handle: catch?, fail?
    #icode_begin_handler{} -> [Instruction];
    #icode_begin_try{} ->     [Instruction];
    #icode_comment{} ->       [Instruction];
    #icode_end_try{} ->       [Instruction];
    #icode_fail{} ->          [Instruction];
    #icode_goto{} ->          [Instruction];
    #icode_label{} ->         [Instruction];
    #icode_return{} ->        [Instruction]
  end.

%%-----------------------------------------------------------------------------

update_call(Instruction, Environment) ->
  DestList = hipe_icode:call_dstlist(Instruction),
  case DestList of
    [Destination] ->
      case lookup_lattice_value(Destination, Environment) of
	bottom ->
	  NewArguments = update_arguments(
			   hipe_icode:call_args(Instruction),
			   Environment),
	  [hipe_icode:call_args_update(Instruction, NewArguments)];
	X -> 
	  NewInstructions =
	    case is_call_to_fp_op(Instruction) of
	      true ->
		TmpIns = 
		  hipe_icode:call_fun_update(Instruction, unsafe_untag_float),
		[hipe_icode:call_args_update(TmpIns, [X])];
	      false ->
		case hipe_icode:call_continuation(Instruction) of
		  [] ->
		    [hipe_icode:mk_move(Destination, X)];
		  ContinuationLabel ->
		    [hipe_icode:mk_move(Destination, X),
		     hipe_icode:mk_goto(ContinuationLabel)]
		end
	    end,
	  ?CONST_PROP_MSG("call: ~w ---> ~w\n",
			  [Instruction, NewInstructions]),
	  NewInstructions
      end;
    %% [] ->  %% No destination; we don't touch this
    %% List-> %% Means register allocation; not implemented at this point
    _ ->
      NewArguments = update_arguments(hipe_icode:call_args(Instruction),
                                      Environment),
      [hipe_icode:call_args_update(Instruction, NewArguments)]
  end.

%%-----------------------------------------------------------------------------

is_call_to_fp_op(Instruction) ->
  case hipe_icode:call_fun(Instruction) of
    fp_add             -> true;
    fp_sub             -> true;
    fp_mul             -> true;
    fp_div             -> true;
    fnegate            -> true;
    conv_to_float      -> true;
    unsafe_untag_float -> true;
    _                  -> false
  end.

%%-----------------------------------------------------------------------------

update_enter(Instruction, Environment) ->
  Args = hipe_icode:enter_args(Instruction),
  EvalArgs = [lookup_lattice_value(X, Environment) || X <- Args],
  Fun = hipe_icode:enter_fun(Instruction),
  case lists:any(fun(X) -> (X =:= bottom) end, EvalArgs) of
    true ->
      update_enter_arguments(Instruction, Environment);
    false ->
      ConstVals = [hipe_icode:const_value(X) || X <- EvalArgs],
      try evaluate_call_or_enter(ConstVals, Fun) of
	bottom -> 
	  update_enter_arguments(Instruction, Environment);
	Const ->
	  Dst = hipe_icode:mk_new_var(),
	  [hipe_icode:mk_move(Dst, Const),
	   hipe_icode:mk_return([Dst])]
      catch
	_:_ -> 
	  update_enter_arguments(Instruction, Environment)
      end
  end.

update_enter_arguments(Instruction, Env) ->
  NewArguments = update_arguments(hipe_icode:enter_args(Instruction), Env),
  [hipe_icode:enter_args_update(Instruction, NewArguments)].

%%-----------------------------------------------------------------------------

update_if(Instruction, Environment) ->
  Args = hipe_icode:if_args(Instruction),
  EvaluatedArguments = [lookup_lattice_value(Argument, Environment) 
                        || Argument <- Args],
  Op = hipe_icode:if_op(Instruction),
  case evaluate_if(Op, EvaluatedArguments) of
    true ->
      TrueLabel  = hipe_icode:if_true_label(Instruction),
      ?CONST_PROP_MSG("ifT: ~w ---> goto ~w\n", [Instruction, TrueLabel]),
      [hipe_icode:mk_goto(TrueLabel)];
    false ->
      FalseLabel = hipe_icode:if_false_label(Instruction),
      ?CONST_PROP_MSG("ifF: ~w ---> goto ~w\n", [Instruction, FalseLabel]),
      [hipe_icode:mk_goto(FalseLabel)];
    bottom ->
      %% Convert the if-test to a type test if possible.
      Op = hipe_icode:if_op(Instruction),
      case Op =:= '=:=' orelse Op =:= '=/=' of
	false ->
	  [hipe_icode:if_args_update(
	     Instruction, update_arguments(Args, Environment))];
	true ->
	  [Arg1, Arg2] = Args,
	  case EvaluatedArguments of
	    [bottom, bottom] -> 
	      [Instruction];
	    [bottom, X] -> 
	      conv_if_to_type(Instruction, hipe_icode:const_value(X), Arg1);
	    [X, bottom] ->
	      conv_if_to_type(Instruction, hipe_icode:const_value(X), Arg2)
	  end
      end
  end.

conv_if_to_type(I, Const, Arg) when is_atom(Const);
				    is_integer(Const);
				    Const =:= [] ->
  Test =
    if is_atom(Const) -> {atom, Const};
       is_integer(Const) -> {integer, Const};
       true -> nil
    end,
  {T, F} =
    case hipe_icode:if_op(I) of
      '=:=' -> {hipe_icode:if_true_label(I),hipe_icode:if_false_label(I)};
      '=/=' -> {hipe_icode:if_false_label(I),hipe_icode:if_true_label(I)}
    end,
  NewI = hipe_icode:mk_type([Arg], Test, T, F),
  ?CONST_PROP_MSG("if: ~w ---> type ~w\n", [I, NewI]),
  [NewI];
conv_if_to_type(I, Const, Arg) ->
  %% Note: we are potentially commuting the (equality) comparison here
  [hipe_icode:if_args_update(I, [Arg, hipe_icode:mk_const(Const)])].

%%-----------------------------------------------------------------------------

update_move(Instruction, Environment) ->
  Destination = hipe_icode:move_dst(Instruction),
  case lookup_lattice_value(Destination, Environment) of
    bottom ->
      [Instruction];
    X ->
      case hipe_icode:move_src(Instruction) of
	X ->
	  [Instruction];
	_ ->
	  ?CONST_PROP_MSG("move: ~w ---> ~w\n", [Instruction, X]),
	  [hipe_icode:move_src_update(Instruction, X)]
      end
      %% == [hipe_icode:mk_move(Destination, X)]
  end.

%%-----------------------------------------------------------------------------

update_phi(Instruction, Environment) ->
  Destination = hipe_icode:phi_dst(Instruction),
  case lookup_lattice_value(Destination, Environment) of
    bottom ->
      [Instruction];
    X ->
      ?CONST_PROP_MSG("phi: ~w ---> ~w\n", [Instruction, X]),
      [hipe_icode:mk_move(Destination, X)]
  end.

%%-----------------------------------------------------------------------------

update_type(Instruction, Environment) ->
  EvaluatedArguments = [lookup_lattice_value(Argument, Environment) || 
                         Argument <- hipe_icode:type_args(Instruction)],
  case evaluate_type(hipe_icode:type_test(Instruction), EvaluatedArguments) of
    true ->
      TrueLabel  = hipe_icode:type_true_label(Instruction),
      ?CONST_PROP_MSG("typeT: ~w ---> goto ~w\n", [Instruction, TrueLabel]),
      [hipe_icode:mk_goto(TrueLabel)];
    false ->
      FalseLabel = hipe_icode:type_false_label(Instruction),
      ?CONST_PROP_MSG("typeF: ~w ---> goto ~w\n", [Instruction, FalseLabel]),
      [hipe_icode:mk_goto(FalseLabel)];
    bottom ->
      [Instruction]
  end.

%%-----------------------------------------------------------------------------

update_switch_val(Instruction, Environment) ->
  Argument = hipe_icode:switch_val_term(Instruction),
  Value    = lookup_lattice_value(Argument, Environment),
  case Value of
    bottom ->
      [Instruction];
    _ ->
      Cases     = hipe_icode:switch_val_cases(Instruction),
      FailLabel = hipe_icode:switch_val_fail_label(Instruction),
      Target    = get_switch_target(Cases, Value, FailLabel),
      ?CONST_PROP_MSG("sv: ~w ---> goto ~w\n", [Instruction, Target]),
      [hipe_icode:mk_goto(Target)]
  end.

%%-----------------------------------------------------------------------------

update_switch_tuple_arity(Instruction, Environment) ->
  Argument = hipe_icode:switch_tuple_arity_term(Instruction),
  Value    = lookup_lattice_value(Argument, Environment),
  case Value of
    bottom ->
      [Instruction];
    Constant ->
      UnTagged = hipe_icode:const_value(Constant),
      case is_tuple(UnTagged) of
	true ->
	  Cases     = hipe_icode:switch_tuple_arity_cases(Instruction),
	  FailLabel = hipe_icode:switch_tuple_arity_fail_label(Instruction),
	  Target = get_switch_target(Cases, tuple_size(UnTagged), FailLabel),
	  ?CONST_PROP_MSG("sta: ~w ---> goto ~w\n", [Instruction, Target]),
	  [hipe_icode:mk_goto(Target)];
	false ->
	  [Instruction]
          %% TODO: Can the above be replaced with below??? Perhaps
	  %% together with some sort of "generate failure".
	  %% [hipe_icode:mk_goto(FailLabel)]
      end
  end.

%%-----------------------------------------------------------------------------

lookup_lattice_value(X, Environment) ->
  LatticeValues = env__lattice_values(Environment),
  case hipe_icode:is_const(X) of
    true ->
      X;
    false ->
      case gb_trees:lookup(X, LatticeValues) of
	none ->
	  ?WARNING_MSG("Earlier compiler steps generated erroneous " 
		       "code for X = ~w. We are ignoring this.\n",[X]),
	  bottom;
	{value, top} ->
	  ?EXIT({"lookup_lattice_value, top", X});
	{value, Y} ->
	  Y
      end
  end.

%%-----------------------------------------------------------------------------

update_arguments(ArgumentList, Environment) ->
  [case lookup_lattice_value(X, Environment) of
     bottom ->
       X;
     Constant ->
       Constant
   end || X <- ArgumentList].

%%----------------------------- End of file -----------------------------------
