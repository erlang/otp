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
%%=======================================================================
%% File        : hipe_icode2rtl.erl
%% Author(s)   : Erik Johansson
%% Description : Translates Icode to RTL
%%=======================================================================
%% TODO: Better handling of switches...

-module(hipe_icode2rtl).

-export([translate/2]).
-export([translate_instrs/4]).  %% used in hipe_rtl_mk_switch

%%-------------------------------------------------------------------------

%% -define(DEBUG,1).	% used by hipe.hrl below

-include("../main/hipe.hrl").
-include("../icode/hipe_icode.hrl").
-include("hipe_literals.hrl").

%%-------------------------------------------------------------------------

%% @spec translate(IcodeRecord::#icode{}, Options::options()) -> term()
%%
%%     options() = [option()]
%%     option() = term()
%%
%% @doc Translates a linear form of Icode for a single function to a
%% linear form of RTL-code.
%%
translate(IcodeRecord = #icode{}, Options) ->
  ?IF_DEBUG_LEVEL(2, put(hipe_mfa, hipe_icode:icode_fun(IcodeRecord)), ok),
  %% hipe_icode_pp:pp(Fun),

  %% Initialize gensym and varmap
  {Args, VarMap} = hipe_rtl_varmap:init(IcodeRecord),
  %% Get the name and other info of the function to translate.
  MFA = hipe_icode:icode_fun(IcodeRecord),
  ConstTab = hipe_consttab:new(), % hipe_icode:icode_data(IcodeRecord),
  %% io:format("~w\n", [ConstTab]),
  Icode = hipe_icode:icode_code(IcodeRecord),
  IsClosure = hipe_icode:icode_is_closure(IcodeRecord), 
  IsLeaf = hipe_icode:icode_is_leaf(IcodeRecord),
  IcodeInfo = hipe_icode:icode_info(IcodeRecord),

  %% Translate Icode instructions to RTL instructions
  ?opt_start_timer("Icode to nested RTL"),
  {Code, _VarMap1, ConstTab1} = 
    translate_instrs(Icode, VarMap, ConstTab, Options),
  ?opt_stop_timer("Icode to nested RTL"),
  %% We build the code as list of lists of...
  %%  in order to avoid appends.
  ?opt_start_timer("Flatten RTL"),
  Code1 = lists:flatten(Code), 
  ?opt_stop_timer("Flatten RTL"),
  %% Build the RTL structure.
  Rtl = hipe_rtl:mk_rtl(MFA,
			Args,
			IsClosure,
			IsLeaf,
			Code1,
			ConstTab1,
			{1, hipe_gensym:get_var(rtl)},
			{1, hipe_gensym:get_label(rtl)}),
  %% hipe_rtl:pp(Rtl),
  %% Propagate info from Icode to RTL.
  hipe_rtl:rtl_info_update(Rtl, IcodeInfo).

%%-------------------------------------------------------------------------
 
%%
%% @doc Translates a list of Icode instructions to a list of RTL instructions.
%%
translate_instrs(Is, VarMap, ConstTab, Options) ->
  translate_instrs(Is, VarMap, [], ConstTab, Options).

translate_instrs([], VarMap, RTL_Code, ConstTab, _Options) ->
  {RTL_Code, VarMap, ConstTab};
translate_instrs([I|Is], VarMap, AccCode, ConstTab, Options) ->
  %% Translate one instruction. 
  {Code, VarMap0, ConstTab0} = 
    translate_instruction(I, VarMap, ConstTab, Options),
  %% ?IF_DEBUG_LEVEL(3,?msg("  To Instr: ~w~n",[Code]),no_debug),
  ?IF_DEBUG(?when_option(rtl_show_translation, Options,
			 ?msg("  To Instr: ~w~n", [Code])), ok),
  translate_instrs(Is, VarMap0, [AccCode,Code], ConstTab0, Options).

%%
%% @doc Translates an Icode instruction to one or more RTL instructions.
%%

translate_instruction(I, VarMap, ConstTab, Options) ->
  %% ?IF_DEBUG_LEVEL(3,?msg("From Instr: ~w~n",[I]),no_debug),
  ?IF_DEBUG(?when_option(rtl_show_translation, Options,
			 ?msg("From Instr: ~w~n", [I])), ok),
  case I of
    #icode_call{} ->  
      gen_call(I, VarMap, ConstTab);
    #icode_comment{} ->
      {hipe_rtl:mk_comment(hipe_icode:comment_text(I)), VarMap, ConstTab};  
    #icode_enter{} -> 
      gen_enter(I, VarMap, ConstTab);
    #icode_fail{} ->
      gen_fail(I, VarMap, ConstTab);
    #icode_goto{} -> 
      gen_goto(I, VarMap, ConstTab);
    #icode_if{} ->  
      gen_if(I, VarMap, ConstTab);
    #icode_label{} ->
      gen_label(I, VarMap, ConstTab);
    #icode_move{} ->  
      gen_move(I, VarMap, ConstTab);
    #icode_begin_handler{} ->
      hipe_rtl_exceptions:gen_begin_handler(I, VarMap, ConstTab);
    #icode_return{} -> 
      gen_return(I, VarMap, ConstTab);
    #icode_switch_val{} -> 
      gen_switch_val(I, VarMap, ConstTab, Options);
    #icode_switch_tuple_arity{} -> 
      gen_switch_tuple(I, VarMap, ConstTab, Options);
    #icode_type{} -> 
      gen_type(I, VarMap, ConstTab);
    X ->
      exit({?MODULE,{"unknown Icode instruction",X}})
  end.

%%-------------------------------------------------------------------------

%%
%% CALL
%%

gen_call(I, VarMap, ConstTab) ->
  Fun = hipe_icode:call_fun(I),
  {Dst, VarMap0} = hipe_rtl_varmap:ivs2rvs(hipe_icode:call_dstlist(I), VarMap),
  Fail = hipe_icode:call_fail_label(I),
  
  {Args, VarMap1, ConstTab1, InitCode} = 
    args_to_vars(hipe_icode:call_args(I), VarMap0, ConstTab),
  
  IsGuard = hipe_icode:call_in_guard(I),
   
  {FailLblName, VarMap3} =
    case Fail of 
      [] -> %% Not in a catch
	{[], VarMap1};
      _ ->
	{FLbl, VarMap2} = 
	  hipe_rtl_varmap:icode_label2rtl_label(Fail, VarMap1),
	{hipe_rtl:label_name(FLbl), VarMap2}
    end,
  
  {ContLblName, ContLbl, VarMap4} =
    case hipe_icode:call_continuation(I) of
      [] -> %% This call does not end a BB.
	CLbl = hipe_rtl:mk_new_label(),
	{hipe_rtl:label_name(CLbl), CLbl, VarMap3};
      Cont ->
	{CLbl, NewVarMap} =
	  hipe_rtl_varmap:icode_label2rtl_label(Cont, VarMap3),
	{hipe_rtl:label_name(CLbl), [], NewVarMap}
    end,
	
  {Code, ConstTab2} =
    case hipe_icode:call_type(I) of 
      primop ->
	hipe_rtl_primops:gen_primop(
	  {Fun, Dst, Args, ContLblName, FailLblName},
	  IsGuard, ConstTab1);
      Type ->
	Call = gen_call_1(Fun, Dst, Args, IsGuard, ContLblName,
			  FailLblName, Type),
	{Call, ConstTab1}
    end,
  {[InitCode,Code,ContLbl], VarMap4, ConstTab2}.

%% This catches those standard functions that we inline expand

gen_call_1(Fun={_M,_F,_A}, Dst, Args, IsGuard, Cont, Fail, Type) ->
  case hipe_rtl_primops:gen_call_builtin(Fun, Dst, Args, IsGuard, Cont,
					 Fail) of
    [] ->
      hipe_rtl:mk_call(Dst, Fun, Args, Cont, Fail, conv_call_type(Type));
    Code ->
      Code
  end.

conv_call_type(remote) -> remote;
conv_call_type(local) -> not_remote.

%% --------------------------------------------------------------------

%%
%% ENTER
%%

gen_enter(I, VarMap, ConstTab) ->
  Fun = hipe_icode:enter_fun(I),
  {Args, VarMap1, ConstTab1, InitCode} = 
    args_to_vars(hipe_icode:enter_args(I), VarMap, ConstTab),
  {Code1, ConstTab2} =
    case hipe_icode:enter_type(I) of
      primop ->
	IsGuard = false, % enter cannot happen in a guard
	hipe_rtl_primops:gen_enter_primop({Fun, Args}, IsGuard, ConstTab1);
      Type ->
	Call = gen_enter_1(Fun, Args, Type),
	{Call, ConstTab1}
    end,
  {[InitCode,Code1], VarMap1, ConstTab2}.

%% This catches those standard functions that we inline expand

gen_enter_1(Fun, Args, Type) ->
  case hipe_rtl_primops:gen_enter_builtin(Fun, Args) of
    [] ->
      hipe_rtl:mk_enter(Fun, Args, conv_call_type(Type));
    Code ->
      Code
  end.

%% --------------------------------------------------------------------

%%
%% FAIL
%%

gen_fail(I, VarMap, ConstTab) ->
  Fail = hipe_icode:fail_label(I),
  {Label, VarMap0} =
    if Fail =:= [] ->
	%% not in a catch
	{[], VarMap};
       true ->
	{Lbl, Map} = hipe_rtl_varmap:icode_label2rtl_label(Fail, VarMap),
	{hipe_rtl:label_name(Lbl), Map}
    end,
  {Args, VarMap1, ConstTab1, InitCode} = 
    args_to_vars(hipe_icode:fail_args(I), VarMap0, ConstTab),
  Class = hipe_icode:fail_class(I),
  FailCode = hipe_rtl_exceptions:gen_fail(Class, Args, Label),
  {[InitCode, FailCode], VarMap1, ConstTab1}.

%% --------------------------------------------------------------------

%%
%% GOTO
%%

gen_goto(I, VarMap, ConstTab) ->
  {Label, Map0} = 
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:goto_label(I), VarMap),
  {hipe_rtl:mk_goto(hipe_rtl:label_name(Label)), Map0, ConstTab}.

%% --------------------------------------------------------------------

%%
%% IF
%%

gen_if(I, VarMap, ConstTab) ->
  {Args, VarMap1, ConstTab1, InitCode} = 
    args_to_vars(hipe_icode:if_args(I), VarMap, ConstTab),
  {TrueLbl, VarMap2} = 
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:if_true_label(I), VarMap1),
  {FalseLbl, VarMap3} = 
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:if_false_label(I),VarMap2),
  CondCode = 
    gen_cond(hipe_icode:if_op(I),
	     Args,
	     hipe_rtl:label_name(TrueLbl),
	     hipe_rtl:label_name(FalseLbl),
	     hipe_icode:if_pred(I)),
  {[InitCode,CondCode], VarMap3, ConstTab1}.


%% --------------------------------------------------------------------

%%
%% LABEL
%%

gen_label(I, VarMap, ConstTab) ->
  LabelName = hipe_icode:label_name(I),
  {NewLabel,Map0} = hipe_rtl_varmap:icode_label2rtl_label(LabelName, VarMap),
  {NewLabel,Map0,ConstTab}.

%% --------------------------------------------------------------------

%%
%% MOVE
%%

gen_move(I, VarMap, ConstTab) ->
  MovedSrc = hipe_icode:move_src(I),
  {Dst, VarMap0} =
    hipe_rtl_varmap:icode_var2rtl_var(hipe_icode:move_dst(I), VarMap),
  case hipe_icode:is_const(MovedSrc) of
    true ->
      {Code, NewConstMap} = gen_const_move(Dst, MovedSrc, ConstTab),
      {[Code], VarMap0, NewConstMap};
    false ->
      {Src, VarMap1} = hipe_rtl_varmap:icode_var2rtl_var(MovedSrc, VarMap0),
      Code = 
	case  hipe_icode:is_fvar(MovedSrc) of
	  true ->
	    hipe_rtl:mk_fmove(Dst, Src);
	  false -> % It is a var or reg
	    hipe_rtl:mk_move(Dst, Src)
	end,
      {[Code], VarMap1, ConstTab}
  end.

%% --------------------------------------------------------------------

%%
%% RETURN
%%

gen_return(I, VarMap, ConstTab) ->
  {RetVars, VarMap0, ConstTab0, Code} = 
    args_to_vars(hipe_icode:return_vars(I), VarMap, ConstTab),
  {Code ++ [hipe_rtl:mk_return(RetVars)], VarMap0, ConstTab0}.

%% --------------------------------------------------------------------

%%
%% SWITCH
%%

%%
%% Rewrite switch_val to the equivalent Icode if-then-else sequence,
%% then translate that sequence instead.
%% Doing this at the RTL level would generate the exact same code,
%% but would also require _a_lot_ more work.
%% (Don't believe me? Try it. I did, and threw the code away in disgust.
%% The main ugliness comes from (1) maintaining ConstTab for the constants
%% that may be added there [switch_val is not limited to immediates!],
%% (2) maintaining Map for the translated labels, and (3) expanding
%% equality tests to eq-or-call-primop-exact_eqeq_2.)
%%
%% TODO:
%% - separate immediate and non-immediate cases,
%%   and translate each list separately
%%
-ifdef(usesjumptable).
-define(uumess,?msg("~w Use jtab: ~w\n",
		    [Options,proplists:get_bool(use_jumptable, Options)])).
-else.
-define(uumess,ok).
-endif.

gen_switch_val(I, VarMap, ConstTab, Options) ->
  %% If you want to see whether jumptables are used or not...
  ?uumess,
  hipe_rtl_mk_switch:gen_switch_val(I, VarMap, ConstTab, Options).

gen_switch_tuple(I, Map, ConstTab, Options) ->
  hipe_rtl_mk_switch:gen_switch_tuple(I, Map, ConstTab, Options).

%% --------------------------------------------------------------------

%%
%% TYPE
%%

gen_type(I, VarMap, ConstTab) ->
  {Vars, Map0, NewConstTab, Code1} = 
    args_to_vars(hipe_icode:type_args(I), VarMap, ConstTab),
  {TrueLbl, Map1} =
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:type_true_label(I), Map0),
  {FalseLbl, Map2} =
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:type_false_label(I), Map1),
  {Code2, NewConstTab1} = gen_type_test(Vars, hipe_icode:type_test(I), 
					hipe_rtl:label_name(TrueLbl),
					hipe_rtl:label_name(FalseLbl),
					hipe_icode:type_pred(I),
					NewConstTab),
  {Code1 ++ Code2, Map2, NewConstTab1}.

%% --------------------------------------------------------------------

%%
%% Generate code for a type test. If X is not of type Type then goto Label.
%%

gen_type_test([X], Type, TrueLbl, FalseLbl, Pred, ConstTab) ->
  case Type of
    atom ->
      {hipe_tagscheme:test_atom(X, TrueLbl, FalseLbl, Pred), ConstTab};
    bignum ->
      {hipe_tagscheme:test_bignum(X, TrueLbl, FalseLbl, Pred), ConstTab};
    binary ->
      {hipe_tagscheme:test_binary(X, TrueLbl, FalseLbl, Pred), ConstTab};
    bitstr ->
      {hipe_tagscheme:test_bitstr(X, TrueLbl, FalseLbl, Pred), ConstTab};
    boolean ->
      TmpT = hipe_rtl:mk_new_var(),
      TmpF = hipe_rtl:mk_new_var(),
      Lbl = hipe_rtl:mk_new_label(),
      {[hipe_rtl:mk_load_atom(TmpT, true),
	hipe_rtl:mk_branch(X, eq, TmpT, TrueLbl,hipe_rtl:label_name(Lbl),Pred),
        Lbl,
        hipe_rtl:mk_load_atom(TmpF, false),
        hipe_rtl:mk_branch(X, eq, TmpF, TrueLbl, FalseLbl, Pred)], ConstTab};
    cons ->
      {hipe_tagscheme:test_cons(X, TrueLbl, FalseLbl, Pred), ConstTab};
    fixnum ->
      {hipe_tagscheme:test_fixnum(X, TrueLbl, FalseLbl, Pred), ConstTab};
    float ->
      {hipe_tagscheme:test_flonum(X, TrueLbl, FalseLbl, Pred), ConstTab};
    function ->
      {hipe_tagscheme:test_fun(X, TrueLbl, FalseLbl, Pred), ConstTab};
    integer ->
      {hipe_tagscheme:test_integer(X, TrueLbl, FalseLbl, Pred), ConstTab};
    list ->
      {hipe_tagscheme:test_list(X, TrueLbl, FalseLbl, Pred), ConstTab};
    map ->
      {hipe_tagscheme:test_map(X, TrueLbl, FalseLbl, Pred), ConstTab};
    nil ->
      {hipe_tagscheme:test_nil(X, TrueLbl, FalseLbl, Pred), ConstTab};
    number ->
      {hipe_tagscheme:test_number(X, TrueLbl, FalseLbl, Pred), ConstTab};
    pid ->
      {hipe_tagscheme:test_any_pid(X, TrueLbl, FalseLbl, Pred), ConstTab};
    port ->
      {hipe_tagscheme:test_any_port(X, TrueLbl, FalseLbl, Pred), ConstTab};
    reference ->
      {hipe_tagscheme:test_ref(X, TrueLbl, FalseLbl, Pred), ConstTab};
    tuple ->
      {hipe_tagscheme:test_tuple(X, TrueLbl, FalseLbl, Pred), ConstTab};
    {atom, Atom} ->
      Tmp = hipe_rtl:mk_new_var(),
      {[hipe_rtl:mk_load_atom(Tmp, Atom),
	hipe_rtl:mk_branch(X, eq, Tmp, TrueLbl, FalseLbl, Pred)], ConstTab};
    {integer, N} when is_integer(N) -> 
      %% XXX: warning, does not work for bignums
      case hipe_tagscheme:is_fixnum(N) of
	true ->
	  Int = hipe_tagscheme:mk_fixnum(N),
	  {hipe_rtl:mk_branch(X, eq, hipe_rtl:mk_imm(Int),
			      TrueLbl, FalseLbl, Pred),
	   ConstTab};
	false ->
	  BignumLbl = hipe_rtl:mk_new_label(),
	  RetLbl = hipe_rtl:mk_new_label(),
	  BigN = hipe_rtl:mk_new_var(),
	  Tmp = hipe_rtl:mk_new_var(),
	  {BigCode,NewConstTab} = gen_big_move(BigN, N, ConstTab),
	  {[hipe_tagscheme:test_fixnum(X, FalseLbl,
				       hipe_rtl:label_name(BignumLbl),1-Pred),
	    BignumLbl, BigCode]
	   ++
	   [hipe_rtl:mk_call([Tmp], op_exact_eqeq_2 , [X,BigN],
			     hipe_rtl:label_name(RetLbl),[],not_remote),
	    RetLbl,
	    hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			       TrueLbl, FalseLbl, Pred)],
	   NewConstTab}
      end;
    {record, A, S} ->
      TupleLbl = hipe_rtl:mk_new_label(),
      TupleLblName = hipe_rtl:label_name(TupleLbl),
      AtomLab = hipe_rtl:mk_new_label(),
      AtomLabName = hipe_rtl:label_name(AtomLab),
      TagVar = hipe_rtl:mk_new_var(),
      TmpAtomVar = hipe_rtl:mk_new_var(),
      {UntagCode, ConstTab1} =
	hipe_rtl_primops:gen_primop({{unsafe_element,1},[TagVar],[X],
				     AtomLabName,[]},
				    false, ConstTab),
      Code = 
	hipe_tagscheme:test_tuple_N(X, S, TupleLblName, FalseLbl, Pred) ++
	[TupleLbl|UntagCode] ++
	[AtomLab,
	 hipe_rtl:mk_load_atom(TmpAtomVar, A),
	 hipe_rtl:mk_branch(TagVar, eq, TmpAtomVar, TrueLbl, FalseLbl, Pred)],
      {Code,
       ConstTab1};
    {tuple, N} ->
      {hipe_tagscheme:test_tuple_N(X, N, TrueLbl, FalseLbl, Pred), ConstTab};
    Other ->
      exit({?MODULE,{"unknown type",Other}})
  end;
gen_type_test(Z = [X,Y], Type, TrueLbl, FalseLbl, Pred, ConstTab) ->
  case Type of
    function2 ->
      {hipe_tagscheme:test_fun2(X, Y, TrueLbl, FalseLbl, Pred), ConstTab};
    fixnum ->
      {hipe_tagscheme:test_fixnums(Z, TrueLbl, FalseLbl, Pred), ConstTab};
    Other ->
      exit({?MODULE,{"unknown type",Other}})
  end;
gen_type_test(X, Type, TrueLbl, FalseLbl, Pred, ConstTab) ->
  case Type of
    fixnum -> 
      {hipe_tagscheme:test_fixnums(X, TrueLbl, FalseLbl, Pred), ConstTab};
    Other ->
      exit({?MODULE,{"type cannot have several arguments",Other}})
  end.


%% --------------------------------------------------------------------
%%
%% Generate code for the if-conditional.
%%

gen_cond(CondOp, Args, TrueLbl, FalseLbl, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  GenLbl = hipe_rtl:mk_new_label(),
  TestRetLbl = hipe_rtl:mk_new_label(),
  TestRetName = hipe_rtl:label_name(TestRetLbl),

  case CondOp of
    'fixnum_eq' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2, TrueLbl,
			  FalseLbl, Pred)];
    '=:=' ->
      [Arg1, Arg2] = Args,
      TypeTestLbl = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_branch(Arg1, eq, Arg2, TrueLbl,
			  hipe_rtl:label_name(TypeTestLbl), Pred),
       TypeTestLbl,
       hipe_tagscheme:test_either_immed(Arg1, Arg2, FalseLbl,
					hipe_rtl:label_name(GenLbl)),
       GenLbl,
       hipe_rtl:mk_call([Tmp], op_exact_eqeq_2, Args,
			TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    'fixnum_neq' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2, FalseLbl,
			  TrueLbl, 1-Pred)];
    '=/=' ->
      [Arg1, Arg2] = Args,
      TypeTestLbl = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_branch(Arg1, eq, Arg2, FalseLbl,
			  hipe_rtl:label_name(TypeTestLbl), 1-Pred),
       TypeTestLbl,
       hipe_tagscheme:test_either_immed(Arg1, Arg2, TrueLbl,
					hipe_rtl:label_name(GenLbl)),
       GenLbl,
       hipe_rtl:mk_call([Tmp], op_exact_eqeq_2, Args,
			TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  FalseLbl, TrueLbl, Pred)];
    '==' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2,
			  TrueLbl, hipe_rtl:label_name(GenLbl), Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, eq, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '/=' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2,
			  FalseLbl, hipe_rtl:label_name(GenLbl), 1-Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    'fixnum_gt' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:fixnum_gt(Arg1, Arg2, TrueLbl, FalseLbl, Pred)];
    'fixnum_ge' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:fixnum_ge(Arg1, Arg2, TrueLbl, FalseLbl, Pred)];
    'fixnum_lt' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:fixnum_lt(Arg1, Arg2, TrueLbl, FalseLbl, Pred)];
    'fixnum_le' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:fixnum_le(Arg1, Arg2, TrueLbl, FalseLbl, Pred)];
    '>' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_gt(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, gt, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '<' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_lt(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, lt, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '>=' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_ge(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ge, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '=<' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_le(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, le, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    _Other ->
      [hipe_rtl:mk_call([Tmp], CondOp, Args, TestRetName, [], not_remote),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)]
  end.

%% --------------------------------------------------------------------
%%
%% Translate a list argument list of icode vars to rtl vars. Also
%% handles constants in arguments.
%% 

args_to_vars([Arg|Args],VarMap, ConstTab) ->
  {Vars, VarMap1, ConstTab1, Code} = 
    args_to_vars(Args, VarMap, ConstTab),
  case hipe_icode:is_variable(Arg) of
    true ->
      {Var, VarMap2} = hipe_rtl_varmap:icode_var2rtl_var(Arg, VarMap1),
      {[Var|Vars], VarMap2, ConstTab1, Code};
    false ->
      case type_of_const(Arg) of
	big ->
	  ConstVal = hipe_icode:const_value(Arg),
	  {ConstTab2, Label} = hipe_consttab:insert_term(ConstTab1, ConstVal),
	  NewArg = hipe_rtl:mk_const_label(Label),
	  {[NewArg|Vars], VarMap1, ConstTab2, Code};
	fixnum ->
	  ConstVal = hipe_icode:const_value(Arg),
	  NewArg = hipe_rtl:mk_imm(tagged_val_of(ConstVal)),
	  {[NewArg|Vars], VarMap1, ConstTab1, Code};
	nil ->
	  NewArg = hipe_rtl:mk_imm(tagged_val_of([])),
	  {[NewArg|Vars], VarMap1, ConstTab1, Code};
	_ ->
	  Var = hipe_rtl:mk_new_var(),
	  {Code2, ConstTab2} = gen_const_move(Var, Arg, ConstTab1),
	  {[Var|Vars], VarMap1, ConstTab2, [Code2,Code]}
      end
  end;
args_to_vars([], VarMap, ConstTab) ->
  {[], VarMap, ConstTab, []}.

%% --------------------------------------------------------------------

%%
%% Translate a move where the source is a constant
%%

gen_const_move(Dst, Const, ConstTab) ->
  ConstVal = hipe_icode:const_value(Const),
  case type_of_const(Const) of
    %% const_fun -> 
    %%   gen_fun_move(Dst, ConstVal, ConstTab);
    nil ->
      Src = hipe_rtl:mk_imm(tagged_val_of([])),
      {hipe_rtl:mk_move(Dst, Src), ConstTab};
    fixnum ->
      Src = hipe_rtl:mk_imm(tagged_val_of(ConstVal)),
      {hipe_rtl:mk_move(Dst, Src), ConstTab};
    atom ->
      {hipe_rtl:mk_load_atom(Dst, ConstVal), ConstTab};
    big ->
      gen_big_move(Dst, ConstVal, ConstTab)
  end.

%% gen_fun_move(Dst, Fun, ConstTab) ->
%%   ?WARNING_MSG("Funmove ~w! -- NYI\n", [Fun]),
%%   {NewTab, Label} = hipe_consttab:insert_fun(ConstTab, Fun),
%%   {hipe_rtl:mk_load_address(Dst, Label, constant), NewTab}.

gen_big_move(Dst, Big, ConstTab) ->
  {NewTab, Label} = hipe_consttab:insert_term(ConstTab, Big),
  {hipe_rtl:mk_move(Dst, hipe_rtl:mk_const_label(Label)),
   NewTab}.

type_of_const(Const) ->
  case hipe_icode:const_value(Const) of
    [] -> 
      nil;
    X when is_integer(X) ->
      case hipe_tagscheme:is_fixnum(X) of
	true -> fixnum;
	false -> big
      end;
    A when is_atom(A) ->
      atom;
    _ -> 
      big
  end.

tagged_val_of([]) -> hipe_tagscheme:mk_nil();
tagged_val_of(X) when is_integer(X) -> hipe_tagscheme:mk_fixnum(X).
