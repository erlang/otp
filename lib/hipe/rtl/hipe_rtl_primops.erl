%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_rtl_primops.erl
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-03-15 Erik Johansson (happi@it.uu.se): 
%%               Created.
%%
%% $Id$
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_primops). 

-export([gen_primop/3, gen_enter_primop/3, gen_call_builtin/6,
	 gen_enter_builtin/2]).

%% --------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("../icode/hipe_icode_primops.hrl").
-include("hipe_rtl.hrl").
-include("hipe_literals.hrl").

%% --------------------------------------------------------------------
%% Handling of known MFA builtins that are inline expanded

gen_call_builtin(Fun, Dst, Args, IsGuard, Cont, Fail) ->
  case Fun of
    {erlang, apply, 3} ->
      gen_apply(Dst, Args, Cont, Fail);

    {erlang, element, 2} ->
      gen_element(Dst, Args, IsGuard, Cont, Fail);

    {erlang, self, 0} ->
      gen_self(Dst, Cont);

    {erlang, is_tuple, 1} ->
      gen_is_tuple(Dst, Args, Cont);

    {hipe_bifs, in_native, 0} ->
      Dst1 =
	case Dst of
	  [] -> %% The result is not used.
	    hipe_rtl:mk_new_var();
	  [Dst0] -> Dst0
	end,
      [hipe_rtl:mk_load_atom(Dst1, true), hipe_rtl:mk_goto(Cont)];
    
    _ -> []    % not a builtin
  end.

%% (Recall that enters cannot occur within a catch-region in the same
%% function, so we do not need to consider fail-continuations here.)
%% TODO: should we inline expand more functions here? Cf. above.
gen_enter_builtin(Fun, Args) ->
  case Fun of
    {erlang, apply, 3} ->
      gen_enter_apply(Args);

%% TODO
%%     {erlang, element, 2} ->
%%       gen_enter_element(Args, IsGuard);

%% TODO
%%     {erlang, self, 0} ->
%%       gen_enter_self();

    {hipe_bifs, in_native, 0} ->
      Dst = hipe_rtl:mk_new_var(),
      [hipe_rtl:mk_load_atom(Dst, true), hipe_rtl:mk_return([Dst])];

    _ -> []    % not a builtin
  end.

%% --------------------------------------------------------------------
%% Generate code to jump to in case the inlined function fails.

gen_fail_code(Fail, Type) -> 
  gen_fail_code(Fail, Type, false).

gen_fail_code(Fail, Type, IsGuard) -> 
  case IsGuard of
    true when Fail =/= [] ->
      {Fail, []};  % go directly to target
    false ->
      NewLabel =  hipe_rtl:mk_new_label(),
      NewLabelName = hipe_rtl:label_name(NewLabel),
      {NewLabelName, [NewLabel | fail_code(Fail, Type)]}
  end.

fail_code(Fail, Type) when is_atom(Type) ->
  Var = hipe_rtl:mk_new_var(),
  [hipe_rtl:mk_load_atom(Var, Type),
   hipe_rtl_exceptions:gen_fail(error, [Var], Fail)];
fail_code(Fail, {Type, Value}) when is_atom(Type) ->
  Var = hipe_rtl:mk_new_var(),
  [hipe_rtl:mk_load_atom(Var, Type),
   hipe_rtl:mk_gctest(3),  % room for a 2-tuple
   gen_mk_tuple(Var,[Var,Value]),
   hipe_rtl_exceptions:gen_fail(error, [Var], Fail)].

fp_fail_code(TmpFailLbl, FailLbl) ->
  [TmpFailLbl |
   hipe_rtl_arch:handle_fp_exception() ++
   [fail_code(FailLbl, badarith)]].

%% --------------------------------------------------------------------
%% CALL PRIMOP
%%
%% @doc
%%   Generates RTL code for primops. This is mostly a dispatch function.
%%   Tail calls to primops (enter_fun, apply, etc.) are not handled here!
%% @end

gen_primop({Op,Dst,Args,Cont,Fail}, IsGuard, ConstTab) ->
  GotoCont = hipe_rtl:mk_goto(Cont),
  case Op of
    %%
    %% Binary Syntax
    %%
    {hipe_bs_primop, BsOP} ->
      {FailLabelName, FailCode1} = gen_fail_code(Fail, badarg, IsGuard),
      {SysLimLblName, FailCode2} = gen_fail_code(Fail, system_limit, IsGuard),
      {Code1,NewConstTab} = 
	hipe_rtl_binary:gen_rtl(BsOP, Dst, Args, Cont, FailLabelName, 
				SysLimLblName, ConstTab),
      {[Code1,FailCode1,FailCode2], NewConstTab};
    %%
    %% Other primops
    %%
    _ ->
      Code = 
	case Op of
	  %% Arithmetic
	  '+' ->
	    %gen_extra_unsafe_add_2(Dst, Args, Cont);
	    gen_add_sub_2(Dst, Args, Cont, Fail, Op, add);
	  '-' ->
	    gen_add_sub_2(Dst, Args, Cont, Fail, Op, sub);
	  '*' ->
	    gen_mul_2(Dst, Args, Cont, Fail);
	  '/' ->
	    %% BIF call: am_Div -> nbif_div_2 -> erts_mixed_div
	    [hipe_rtl:mk_call(Dst, '/', Args, Cont, Fail, not_remote)];
          'gen_add' ->
            gen_general_add_sub(Dst, Args, Cont, Fail, '+');
          'gen_sub' ->
            gen_general_add_sub(Dst, Args, Cont, Fail, '-');
	  'unsafe_add' ->
	    %gen_extra_unsafe_add_2(Dst, Args, Cont);
	    gen_unsafe_add_sub_2(Dst, Args, Cont, Fail, '+', add);
	  'extra_unsafe_add' ->
	    gen_extra_unsafe_add_2(Dst, Args, Cont);
	  'unsafe_sub' ->
	    gen_unsafe_add_sub_2(Dst, Args, Cont, Fail, '-', sub);
	  'extra_unsafe_sub' ->
	    gen_extra_unsafe_sub_2(Dst, Args, Cont);
          %'unsafe_mul' ->
          %  gen_unsafe_mul_2(Dst, Args, Cont, Fail, '*');
	  'div' ->
	    %% BIF call: am_div -> nbif_intdiv_2 -> intdiv_2
	    [hipe_rtl:mk_call(Dst, 'div', Args, Cont, Fail, not_remote)];
	  'rem' ->
	    %% BIF call: am_rem -> nbif_rem_2 -> rem_2
	    [hipe_rtl:mk_call(Dst, 'rem', Args, Cont, Fail, not_remote)];
	  'band' ->
	    gen_bitop_2(Dst, Args, Cont, Fail, Op, 'and');
	  'bor' ->
	    gen_bitop_2(Dst, Args, Cont, Fail, Op, 'or');
	  'bxor' ->
	    gen_bitop_2(Dst, Args, Cont, Fail, Op, 'xor');
	  'bnot' ->
	    gen_bnot_2(Dst, Args, Cont, Fail, Op);
	  'bsr'->
	    %% BIF call: am_bsr -> nbif_bsr_2 -> bsr_2
	    gen_bsr_2(Dst, Args, Cont, Fail, Op);
	    %[hipe_rtl:mk_call(Dst, 'bsr', Args, Cont, Fail, not_remote)];
	  'bsl' -> 
	    %% BIF call: am_bsl -> nbif_bsl_2 -> bsl_2
	    [hipe_rtl:mk_call(Dst, 'bsl', Args, Cont, Fail, not_remote)];
	  unsafe_band ->
	    gen_unsafe_bitop_2(Dst, Args, Cont, 'and');
	  unsafe_bor -> 
	    gen_unsafe_bitop_2(Dst, Args, Cont, 'or');
	  unsafe_bxor ->
	    gen_unsafe_bitop_2(Dst, Args, Cont, 'xor');
	  unsafe_bnot ->
	    gen_unsafe_bnot_2(Dst, Args, Cont);
	  unsafe_bsr ->
	    gen_unsafe_bsr_2(Dst, Args, Cont);
          unsafe_bsl ->
	    gen_unsafe_bsl_2(Dst, Args, Cont);
	  %%---------------------------------------------
	  %% List handling
	  %%---------------------------------------------
	  cons ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->	    
		[gen_cons(Dst1, Args), GotoCont]
	    end;
	  unsafe_hd ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->	  
		[gen_unsafe_hd(Dst1, Args), GotoCont]
	    end;
	  unsafe_tl ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->	 
		[gen_unsafe_tl(Dst1, Args),GotoCont]
	    end;
	  %%---------------------------------------------
	  %% Tuple handling
	  %%---------------------------------------------
	  mktuple ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->
		[gen_mk_tuple(Dst1, Args),GotoCont]
	    end;
	  #unsafe_element{index=N} ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->
		[Tuple] = Args,
		[gen_unsafe_element(Dst1, hipe_rtl:mk_imm(N), Tuple),GotoCont]
	    end;
	  #unsafe_update_element{index=N} ->
	    [Dst1] = Dst,
	    [Tuple, Value] = Args,
	    [gen_unsafe_update_element(Tuple, hipe_rtl:mk_imm(N), Value),
	     hipe_rtl:mk_move(Dst1, Tuple),
	     GotoCont];
	  {element, [TupleInfo, IndexInfo]} ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  hipe_rtl:mk_new_var();
		[Dst0] -> Dst0
	      end,
	    [Index, Tuple] = Args,	    
	    [gen_element_1(Dst1, Index, Tuple, IsGuard, Cont, Fail,
			   TupleInfo, IndexInfo)];

	  %%---------------------------------------------
	  %% Apply-fixarity
	  %%---------------------------------------------
	  #apply_N{arity = Arity} ->
	    gen_apply_N(Dst, Arity, Args, Cont, Fail);

	  %%---------------------------------------------
	  %% GC test
	  %%---------------------------------------------
	  #gc_test{need = Need} ->
	    [hipe_rtl:mk_gctest(Need), GotoCont];

	  %%---------------------------------------------
	  %% Process handling
	  %%---------------------------------------------
	  redtest ->
	    [gen_redtest(1), GotoCont];
	  %%---------------------------------------------
	  %% Receives
	  %%---------------------------------------------
	  check_get_msg ->
	    gen_check_get_msg(Dst, GotoCont, Fail);
	  next_msg ->
	    gen_next_msg(Dst, GotoCont);
	  select_msg ->
	    gen_select_msg(Dst, Cont);
	  clear_timeout ->
	    gen_clear_timeout(Dst, GotoCont);
	  set_timeout ->
	    %% BIF call: am_set_timeout -> nbif_set_timeout -> hipe_set_timeout
	    [hipe_rtl:mk_call(Dst, set_timeout, Args, Cont, Fail, not_remote)];
	  suspend_msg ->
	    gen_suspend_msg(Dst, Cont);
	  %%---------------------------------------------
	  %% Closures
	  %%---------------------------------------------
	  call_fun ->
	    gen_call_fun(Dst, Args, Cont, Fail);
	  #mkfun{mfa=MFA, magic_num=MagicNum, index=Index} ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      _ ->
		[gen_mkfun(Dst, MFA, MagicNum, Index, Args), GotoCont]
	    end;
	  #closure_element{n=N} ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->
		[Closure] = Args,
		[gen_closure_element(Dst1, hipe_rtl:mk_imm(N), Closure),
		 GotoCont]
	    end;
	  %%---------------------------------------------
	  %% Floating point instructions.
	  %%---------------------------------------------
	  fp_add ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fadd', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fadd', Arg2)
	    end;
	  fp_sub ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fsub', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fsub', Arg2)
	    end;	  
	  fp_mul ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fmul', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fmul', Arg2)
	    end;
	  fp_div ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fdiv', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fdiv', Arg2)
	    end;	  
	  fnegate ->
	    [Arg] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp_unop(hipe_rtl:mk_new_fpreg(), Arg, 'fchs');
	      [Dst1] ->
		hipe_rtl:mk_fp_unop(Dst1, Arg, 'fchs')
	    end;	  
	  fclearerror ->
	    gen_fclearerror();
	  fcheckerror ->
	    gen_fcheckerror(Cont, Fail);
	  conv_to_float ->
	    case Dst of
	      [] ->
		gen_conv_to_float(hipe_rtl:mk_new_fpreg(), Args, Cont, Fail);
	      [Dst1] ->
		gen_conv_to_float(Dst1, Args, Cont, Fail)
	    end;
	  unsafe_untag_float ->
	    [Arg] = Args,
	    case Dst of
	      [] ->
		hipe_tagscheme:unsafe_untag_float(hipe_rtl:mk_new_fpreg(),
						  Arg); 
	      [Dst1]->
		hipe_tagscheme:unsafe_untag_float(Dst1, Arg)
	    end;
	  unsafe_tag_float ->
	    [Arg] = Args,
	    case Dst of
	      [] ->
		hipe_tagscheme:unsafe_tag_float(hipe_rtl:mk_new_var(), Arg);
	      [Dst1]->
		hipe_tagscheme:unsafe_tag_float(Dst1, Arg)
	    end;
	  debug_native_called -> 
	    [hipe_rtl:mk_call(Dst, Op, Args, Cont, Fail, not_remote)];

	  %% Only names listed above are accepted! MFA:s are not primops!
	  _ ->
	    erlang:error({bad_primop, Op})
	end,
      {Code, ConstTab}
  end.

gen_enter_primop({Op, Args}, IsGuard, ConstTab) ->
  case Op of
    enter_fun ->
      %% Tail-call to a closure must preserve tail-callness!
      %% (Passing Continuation = [] to gen_call_fun/5 does this.)
      Code = gen_call_fun([], Args, [], []),
      {Code, ConstTab};

    #apply_N{arity=Arity} ->
      %% Tail-call to a closure must preserve tail-callness!
      %% (Passing Continuation = [] to gen_apply_N/5 does this.)
      Code = gen_apply_N([], Arity, Args, [], []),
      {Code, ConstTab};

    _ ->
      %% All other primop tail calls are converted to call + return.
      Dst = [hipe_rtl:mk_new_var()],
      OkLab = hipe_rtl:mk_new_label(),
      {Code,ConstTab1} = 
	gen_primop({Op,Dst,Args,hipe_rtl:label_name(OkLab),[]}, 
		   IsGuard, ConstTab),
      {Code ++ [OkLab, hipe_rtl:mk_return(Dst)], ConstTab1}
  end.


%% --------------------------------------------------------------------
%% ARITHMETIC
%% --------------------------------------------------------------------

%%
%% Inline addition & subtraction
%%

gen_general_add_sub(Dst, Args, Cont, Fail, Op) ->
  case Dst of
    [] ->
      [hipe_rtl:mk_call([hipe_rtl:mk_new_var()],
                        Op, Args, Cont, Fail, not_remote)];
    [Res] ->
      [hipe_rtl:mk_call([Res], Op, Args, Cont, Fail, not_remote)]
  end.

gen_add_sub_2(Dst, Args, Cont, Fail, Op, AluOp) ->
  [Arg1, Arg2] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),
  case Dst of
    [] ->
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenCaseLabel))|
       gen_op_general_case(hipe_rtl:mk_new_var(),
			   Op, Args, Cont, Fail, GenCaseLabel)];
    [Res] ->
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenCaseLabel)),
       hipe_tagscheme:fixnum_addsub(AluOp, Arg1, Arg2, Res, GenCaseLabel)|
       gen_op_general_case(Res,Op, Args, Cont, Fail, GenCaseLabel)]
  end.

gen_unsafe_add_sub_2(Dst, Args, Cont, Fail, Op, AluOp) ->
  [Arg1, Arg2] = Args,
  case Dst of
    [] ->      
      [hipe_rtl:mk_goto(Cont)];
    [Res] ->
      case Fail of
	[]->
	  GenCaseLabel = hipe_rtl:mk_new_label(),
	  [hipe_tagscheme:fixnum_addsub(AluOp, Arg1, Arg2, Res, GenCaseLabel)|
	   gen_op_general_case(Res,Op, Args, Cont, Fail, GenCaseLabel)];
	_ ->
	  [hipe_tagscheme:fixnum_addsub(AluOp, Arg1, Arg2, Res, 
					hipe_rtl:mk_label(Fail))]
      end
  end.

gen_extra_unsafe_add_2(Dst, Args, Cont) ->
  [Arg1, Arg2] = Args,
  case Dst of
    [] ->      
      [hipe_rtl:mk_goto(Cont)];
    [Res] ->
      hipe_tagscheme:unsafe_fixnum_add(Arg1, Arg2, Res)
  end.

gen_extra_unsafe_sub_2(Dst, Args, Cont) ->
  [Arg1, Arg2] = Args,
  case Dst of
    [] ->      
      [hipe_rtl:mk_goto(Cont)];
    [Res] ->
      hipe_tagscheme:unsafe_fixnum_sub(Arg1, Arg2, Res)
  end.

gen_op_general_case(Res, Op, Args, Cont, Fail, GenCaseLabel) ->
  [hipe_rtl:mk_goto(Cont),
   GenCaseLabel,
   hipe_rtl:mk_call([Res], Op, Args, Cont, Fail, not_remote)].

%%
%% Inline multiplication
%%

gen_mul_2(Dst, Args, Cont, Fail) ->
  [Arg1,Arg2] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),
  {Res1,I2} =
    case Dst of
      [] ->
	{hipe_rtl:mk_new_var(), []};
      [Res0] ->
	{Res0, hipe_tagscheme:fixnum_mul(Arg1, Arg2, Res0, GenCaseLabel)}
    end,
  [hipe_tagscheme:test_two_fixnums(Arg1, Arg2, hipe_rtl:label_name(GenCaseLabel)),
   I2,
   %% BIF call: am_Times -> nbif_mul_2 -> erts_mixed_times
   gen_op_general_case(Res1, '*', Args, Cont, Fail, GenCaseLabel)].

%% gen_unsafe_mul_2([Res], Args, Cont, Fail, Op) ->
%%    [Arg1, Arg2] = Args,
%%    GenCaseLabel = hipe_rtl:mk_new_label(),
%%    [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
%% 				    hipe_rtl:label_name(GenCaseLabel)),
%%     hipe_tagscheme:fixnum_mul(Arg1, Arg2, Res, GenCaseLabel)|
%%     gen_op_general_case(Res, Op, Args, Cont, Fail, GenCaseLabel)].

%%
%% Inline bitoperations.
%% Only works for band, bor and bxor.
%% The shift operations are too expensive to inline.
%%

gen_bitop_2(Res, Args, Cont, Fail, Op, BitOp) ->
  [Arg1, Arg2] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),
  case Res of
    [] -> %% The result is not used.
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenCaseLabel))|
       gen_op_general_case(hipe_rtl:mk_new_var(),
			   Op, Args, Cont, Fail, GenCaseLabel)];	
    [Res0] -> 
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenCaseLabel)),
       hipe_tagscheme:fixnum_andorxor(BitOp, Arg1, Arg2, Res0)|
       gen_op_general_case(Res0, Op, Args, Cont, Fail, GenCaseLabel)]
  end.
  
gen_unsafe_bitop_2(Res, Args, Cont, BitOp) ->
  case Res of
    [] -> %% The result is not used.
      [hipe_rtl:mk_goto(Cont)];
    [Res0] -> 
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:fixnum_andorxor(BitOp, Arg1, Arg2, Res0),
       hipe_rtl:mk_goto(Cont)]
  end.

gen_bsr_2(Res, Args, Cont, Fail, Op) ->
  [Arg1, Arg2] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),
  case hipe_rtl:is_imm(Arg2) of
    true ->
      Val =  hipe_tagscheme:fixnum_val(hipe_rtl:imm_value(Arg2)),
      Limit = ?bytes_to_bits(hipe_rtl_arch:word_size()),
      if 
	Val < Limit, Val >= 0 ->
	  case Res of
	    [] ->
	      FixLabel = hipe_rtl:mk_new_label(),
	      [hipe_tagscheme:test_fixnum(Arg1,
					  hipe_rtl:label_name(FixLabel),
					  hipe_rtl:label_name(GenCaseLabel),
					  0.99),
	       FixLabel,
	       gen_op_general_case(hipe_rtl:mk_new_var(), Op, Args, Cont, Fail,
				   GenCaseLabel)];
	    [Res0] ->
	      FixLabel = hipe_rtl:mk_new_label(),
	      [hipe_tagscheme:test_fixnum(Arg1,
					  hipe_rtl:label_name(FixLabel),
					  hipe_rtl:label_name(GenCaseLabel),
					  0.99),
	       FixLabel,
	       hipe_tagscheme:fixnum_bsr(Arg1, Arg2, Res0),
	       gen_op_general_case(Res0, Op, Args, Cont, Fail, GenCaseLabel)]
	  end;
	true ->
	  [hipe_rtl:mk_call(Res, 'bsr', Args, Cont, Fail, not_remote)]
      end;
    false ->
      [hipe_rtl:mk_call(Res, 'bsr', Args, Cont, Fail, not_remote)]
  end.

gen_unsafe_bsr_2(Res, Args, Cont) ->
  case Res of
    [] -> %% The result is not used.
      [hipe_rtl:mk_goto(Cont)];
    [Res0] ->  
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:fixnum_bsr(Arg1, Arg2, Res0),
       hipe_rtl:mk_goto(Cont)]
  end.

gen_unsafe_bsl_2(Res, Args, Cont) ->
  case Res of
    [] -> %% The result is not used.
      [hipe_rtl:mk_goto(Cont)];
    [Res0] ->  
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:fixnum_bsl(Arg1, Arg2, Res0),
       hipe_rtl:mk_goto(Cont)]
  end.

%%
%% Inline not.
%%

gen_bnot_2(Res, Args, Cont, Fail, Op) ->
  [Arg] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),
  case Res of
    [] -> %% The result is not used.
      FixLabel = hipe_rtl:mk_new_label(),
      [hipe_tagscheme:test_fixnum(Arg, hipe_rtl:label_name(FixLabel),
				  hipe_rtl:label_name(GenCaseLabel), 0.99),
       FixLabel,
       gen_op_general_case(hipe_rtl:mk_new_var(), Op, Args, Cont, Fail, 
			   GenCaseLabel)];
    
    [Res0] -> 
      FixLabel = hipe_rtl:mk_new_label(),
      [hipe_tagscheme:test_fixnum(Arg, hipe_rtl:label_name(FixLabel),
				  hipe_rtl:label_name(GenCaseLabel), 0.99),
       FixLabel,
       hipe_tagscheme:fixnum_not(Arg, Res0),
       gen_op_general_case(Res0, Op, Args, Cont, Fail, GenCaseLabel)]
  end.

gen_unsafe_bnot_2(Res, Args, Cont) ->
  case Res of
    [] -> %% The result is not used.
      [hipe_rtl:mk_goto(Cont)];
    [Res0] ->  
      [Arg1] = Args,
      [hipe_tagscheme:fixnum_not(Arg1, Res0),
       hipe_rtl:mk_goto(Cont)]
  end.
 

%% --------------------------------------------------------------------
%% 

%%
%% Inline cons
%%

gen_cons(Dst, [Arg1, Arg2]) ->
  Tmp = hipe_rtl:mk_new_reg(),
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  HeapNeed = 2*WordSize,
  [GetHPInsn,
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Arg1),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(WordSize), Arg2),
   hipe_rtl:mk_move(Tmp, HP),
   hipe_tagscheme:tag_cons(Dst, Tmp),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(HeapNeed)),
   PutHPInsn].

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --------------------------------------------------------------------
%% Handling of closures...
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% gen_mkfun
%%
%%    The gc_test should have expanded to
%%    unsigned needed = ERL_FUN_SIZE + num_free;
%%    ErlFunThing* funp = (ErlFunThing *) HAlloc(p, needed);
%%
%% The code generated should do the equivalent of:
%%  Copy arguments to the fun thing
%%    Eterm* hp = funp->env;
%%    for (i = 0; i < num_free; i++) {
%%	*hp++ = reg[i];
%%    }
%%
%%  Fill in fileds
%%    funp->thing_word = HEADER_FUN;
%%    funp->fe = fe;
%%    funp->num_free = num_free;
%%    funp->creator = p->id;
%%    funp->native_code = fe->native_code;
%%  Increase refcount
%%    fe->refc++;
%%
%%  Link to the process off_heap list
%%    funp->next = p->off_heap.first;
%%    p->off_heap.first = funp;
%%
%%  Tag the thing
%%    return make_fun(funp);
%%
gen_mkfun([Dst], {_Mod, _FunId, _Arity} = MFidA, MagicNr, Index, FreeVars) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  NumFree = length(FreeVars),

  %%  Copy arguments to the fun thing
  %%    Eterm* hp = funp->env;
  %%    for (i = 0; i < num_free; i++) {
  %%	*hp++ = reg[i];
  %%    }
  CopyFreeVarsCode = gen_free_vars(FreeVars, HP),

  %%  Fill in fields
  %%    funp->thing_word = HEADER_FUN;
  %%    funp->fe = fe;
  %%    funp->num_free = num_free;
  %%    funp->creator = p->id;
  %%    funp->native_code = fe->native_code;
  %%  Increase refcount
  %%    fe->refc++;
  SkeletonCode = gen_fun_thing_skeleton(HP, MFidA, NumFree, MagicNr, Index),

  %%  Link to the process off_heap list
  %%    funp->next = p->off_heap.first;
  %%    p->off_heap.first = funp;
  LinkCode = gen_link_closure(HP),

  %%  Tag the thing and increase the heap_pointer.
  %%    make_fun(funp);
  WordSize = hipe_rtl_arch:word_size(),
  HeapNeed = (?ERL_FUN_SIZE + NumFree) * WordSize,
  TagCode = [hipe_tagscheme:tag_fun(Dst, HP), 
	     %%  AdjustHPCode 
	     hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(HeapNeed)),
	     PutHPInsn],
  [[GetHPInsn | CopyFreeVarsCode], SkeletonCode, LinkCode, TagCode].


gen_fun_thing_skeleton(FunP, FunName={_Mod,_FunId,Arity}, NumFree, 
		       MagicNr, Index) ->
  %% Assumes that funp == heap_pointer
  %%  Fill in fields
  %%    funp->thing_word = HEADER_FUN;
  %%    funp->fe = fe;
  %%    funp->num_free = num_free;
  %%    funp->creator = p->id;
  %%    funp->native_code = fe->native_code;
  %%  And creates a fe (at load time).
  FeVar = hipe_rtl:mk_new_reg(),
  PidVar = hipe_rtl:mk_new_reg_gcsafe(),
  NativeVar = hipe_rtl:mk_new_reg(),

  [hipe_rtl:mk_load_address(FeVar, {FunName, MagicNr, Index}, closure),
   store_struct_field(FunP, ?EFT_FE, FeVar),
   load_struct_field(NativeVar, FeVar, ?EFE_NATIVE_ADDRESS),
   store_struct_field(FunP, ?EFT_NATIVE_ADDRESS, NativeVar),

   store_struct_field(FunP, ?EFT_ARITY, hipe_rtl:mk_imm(Arity-NumFree)),

   gen_inc_refc(FeVar, ?EFE_REFC),

   store_struct_field(FunP, ?EFT_NUM_FREE, hipe_rtl:mk_imm(NumFree)),
   load_p_field(PidVar, ?P_ID),
   store_struct_field(FunP, ?EFT_CREATOR, PidVar),
   store_struct_field(FunP, ?EFT_THING, hipe_tagscheme:mk_fun_header())].

gen_inc_refc(Ptr, Offset) ->
  case ?ERTS_IS_SMP of
    0 -> gen_inc_refc_notsmp(Ptr, Offset);
    1 -> gen_inc_refc_smp(Ptr, Offset)
  end.

gen_inc_refc_notsmp(Ptr, Offset) ->
  Refc = hipe_rtl:mk_new_reg(),
  [load_struct_field(Refc, Ptr, Offset, int32),
   hipe_rtl:mk_alu(Refc, Refc, add, hipe_rtl:mk_imm(1)),
   store_struct_field(Ptr, Offset, Refc, int32)].

gen_inc_refc_smp(Ptr, Offset) ->
  Refc = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(Refc, Ptr, 'add', hipe_rtl:mk_imm(Offset)),
   hipe_rtl:mk_call([], 'atomic_inc', [Refc], [], [], not_remote)].

gen_link_closure(FUNP) ->
  case ?P_OFF_HEAP_FUNS of
    [] -> gen_link_closure_non_private(FUNP);
    _ -> gen_link_closure_private(FUNP)
  end.

gen_link_closure_private(FUNP) ->
  %% Link fun to the process off_heap list
  %%   funp->next = p->off_heap.first;
  %%   p->off_heap.first = funp;
  FunsVar = hipe_rtl:mk_new_reg(),

  [load_p_field(FunsVar,?P_OFF_HEAP_FIRST),
   hipe_rtl:mk_store(FUNP, hipe_rtl:mk_imm(?EFT_NEXT), FunsVar),
   store_p_field(FUNP,?P_OFF_HEAP_FIRST)].

gen_link_closure_non_private(_FUNP) -> [].

load_p_field(Dst,Offset) ->
  hipe_rtl_arch:pcb_load(Dst, Offset).
store_p_field(Src, Offset) ->
  hipe_rtl_arch:pcb_store(Offset, Src).

store_struct_field(StructP, Offset, Src) ->
  hipe_rtl:mk_store(StructP, hipe_rtl:mk_imm(Offset), Src).

load_struct_field(Dest, StructP, Offset) ->
  hipe_rtl:mk_load(Dest, StructP, hipe_rtl:mk_imm(Offset)).

store_struct_field(StructP, Offset, Src, int32) ->
  hipe_rtl:mk_store(StructP, hipe_rtl:mk_imm(Offset), Src, int32).

load_struct_field(Dest, StructP, Offset, int32) ->
  hipe_rtl:mk_load(Dest, StructP, hipe_rtl:mk_imm(Offset), int32, signed).

gen_free_vars(Vars, HPReg) ->
  HPVar = hipe_rtl:mk_new_var(),
  WordSize = hipe_rtl_arch:word_size(),
  [hipe_rtl:mk_alu(HPVar, HPReg, add, hipe_rtl:mk_imm(?EFT_ENV)) |
   gen_free_vars(Vars, HPVar, 0, WordSize, [])].

gen_free_vars([Var|Vars], EnvPVar, Offset, WordSize, AccCode) ->
  Code = hipe_rtl:mk_store(EnvPVar, hipe_rtl:mk_imm(Offset), Var),
  gen_free_vars(Vars, EnvPVar, Offset + WordSize, WordSize,
                [Code|AccCode]);
gen_free_vars([], _, _, _, AccCode) -> AccCode.

%% ------------------------------------------------------------------
%%
%% call_fun (also handles enter_fun when Continuation = [])

gen_call_fun(Dst, ArgsAndFun, Continuation, Fail) ->  
  NAddressReg = hipe_rtl:mk_new_reg(),
  ArityReg = hipe_rtl:mk_new_reg_gcsafe(),
  [Fun|RevArgs] = lists:reverse(ArgsAndFun),

  %% {BadFunLabName, BadFunCode} = gen_fail_code(Fail, {badfun, Fun}),
  Args = lists:reverse(RevArgs),
  NonClosureLabel = hipe_rtl:mk_new_label(),
  CallNonClosureLabel = hipe_rtl:mk_new_label(),
  BadFunLabName = hipe_rtl:label_name(NonClosureLabel),
  BadFunCode =
    [NonClosureLabel,
     hipe_rtl:mk_call([NAddressReg],
		      'nonclosure_address',
		      [Fun, hipe_rtl:mk_imm(length(Args))],
		      hipe_rtl:label_name(CallNonClosureLabel),
		      Fail,
		      not_remote),
     CallNonClosureLabel,
     case Continuation of
       [] ->
	 hipe_rtl:mk_enter(NAddressReg, Args, not_remote);
       _ ->
	 hipe_rtl:mk_call(Dst, NAddressReg, Args,
			  Continuation, Fail, not_remote)
     end],

  {BadArityLabName, BadArityCode} = gen_fail_code(Fail, {badarity, Fun}),

  CheckGetCode = 
    hipe_tagscheme:if_fun_get_arity_and_address(ArityReg, NAddressReg,
						Fun, BadFunLabName,
						0.9),
  CheckArityCode = check_arity(ArityReg, length(RevArgs), BadArityLabName),
  CallCode =
    case Continuation of
      [] -> %% This is a tailcall
	[hipe_rtl:mk_enter(NAddressReg, ArgsAndFun, not_remote)];
      _ -> %% Ordinary call
	[hipe_rtl:mk_call(Dst, NAddressReg, ArgsAndFun,
			  Continuation, Fail, not_remote)]
    end,
  [CheckGetCode, CheckArityCode, CallCode, BadFunCode, BadArityCode].

check_arity(ArityReg, Arity, BadArityLab) ->
  TrueLab1 = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_branch(ArityReg, eq, hipe_rtl:mk_imm(Arity),  
		      hipe_rtl:label_name(TrueLab1), BadArityLab, 0.9),
   TrueLab1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% apply
%%
%% The tail call case is not handled here.

gen_apply(Dst, Args = [_M,_F,_AppArgs], Cont, Fail) ->
  %% Dst can be [Res] or [].
  [hipe_rtl:mk_call(Dst, hipe_apply, Args, Cont, Fail, not_remote)].

gen_enter_apply(Args=[_M,_F,_AppArgs]) ->
  %% 'apply' in tail-call context
  [hipe_rtl:mk_enter(hipe_apply, Args, not_remote)].

%%
%% apply_N
%% also handles tailcall case (Cont=[])
%%

gen_apply_N(Dst, Arity, [M,F|CallArgs], Cont, Fail) ->
  MM = hipe_rtl:mk_new_var(),
  NotModuleLbl = hipe_rtl:mk_new_label(),
  NotModuleLblName = hipe_rtl:label_name(NotModuleLbl),
  Tuple = M,
  Index = hipe_rtl:mk_imm(1),
  IndexInfo = 1,
  [hipe_tagscheme:element(MM, Index, Tuple, NotModuleLblName, unknown, IndexInfo),
   gen_apply_N_common(Dst, Arity+1, MM, F, CallArgs ++ [M], Cont, Fail),
   NotModuleLbl,
   gen_apply_N_common(Dst, Arity, M, F, CallArgs, Cont, Fail)].

gen_apply_N_common(Dst, Arity, M, F, CallArgs, Cont, Fail) ->
  CallLabel = hipe_rtl:mk_new_label(),
  CodeAddress = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_call([CodeAddress], find_na_or_make_stub,
		    [M,F,hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(Arity))],
		    hipe_rtl:label_name(CallLabel),
		    Fail, not_remote),
   CallLabel,
   case Cont of
     [] ->	% tailcall
       hipe_rtl:mk_enter(CodeAddress, CallArgs, not_remote);
     _ ->	% recursive call
       hipe_rtl:mk_call(Dst, CodeAddress, CallArgs, Cont, Fail, not_remote)
   end].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% mkTuple
%%

gen_mk_tuple(Dst, Elements) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  Arity = length(Elements),
  WordSize = hipe_rtl_arch:word_size(),
  HeapNeed = (Arity+1)*WordSize,
  [GetHPInsn,
   gen_tuple_header(HP, Arity),
   set_tuple_elements(HP, WordSize, WordSize, Elements, []),
   hipe_tagscheme:tag_tuple(Dst, HP),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(HeapNeed)),
   PutHPInsn].

set_tuple_elements(HP, Offset, WordSize, [Element|Elements], Stores) ->
  Store = hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(Offset), Element),
  set_tuple_elements(HP, Offset+WordSize, WordSize, Elements, [Store|Stores]);
set_tuple_elements(_, _, _, [], Stores) ->
  lists:reverse(Stores).

%%
%% @doc Generate RTL code for the reduction test.
%%
gen_redtest(Amount) ->
  {GetFCallsInsn, FCalls, PutFCallsInsn} = hipe_rtl_arch:fcalls(),
  SuspendLabel = hipe_rtl:mk_new_label(),
  StayLabel = hipe_rtl:mk_new_label(),
  ContinueLabel = hipe_rtl:mk_new_label(),
  [GetFCallsInsn,
   hipe_rtl:mk_alub(FCalls, FCalls, 'sub', hipe_rtl:mk_imm(Amount), 'lt',
		    hipe_rtl:label_name(SuspendLabel),
		    hipe_rtl:label_name(StayLabel), 0.01),
   SuspendLabel,
   %% The suspend path should not execute PutFCallsInsn.
   hipe_rtl:mk_call([], suspend_0, [],
		    hipe_rtl:label_name(ContinueLabel), [], not_remote),
   StayLabel,
   PutFCallsInsn,
   ContinueLabel].

gen_self(Dst, Cont) ->
  case Dst of
    [] -> %% The result is not used.
      [hipe_rtl:mk_goto(Cont)];
    [Dst1] ->
      [load_p_field(Dst1, ?P_ID),
       hipe_rtl:mk_goto(Cont)]
  end.

%%
%% @doc Generate is_tuple/1 test
%%
gen_is_tuple(Dst, [Arg], Cont) ->
  GotoCont = hipe_rtl:mk_goto(Cont),
  case Dst of
    [] -> %% The result is not used.
      [GotoCont];
    [Dst1] ->
      TrueLabel = hipe_rtl:mk_new_label(),
      FalseLabel = hipe_rtl:mk_new_label(),
      [hipe_tagscheme:test_tuple(Arg, hipe_rtl:label_name(TrueLabel),
				 hipe_rtl:label_name(FalseLabel), 0.5),
       TrueLabel,
       hipe_rtl:mk_load_atom(Dst1, true),
       GotoCont,
       FalseLabel,
       hipe_rtl:mk_load_atom(Dst1, false),
       GotoCont]
  end.

%%
%% @doc Generate unsafe head
%%
gen_unsafe_hd(Dst, [Arg]) -> hipe_tagscheme:unsafe_car(Dst, Arg).

%%
%% @doc Generate unsafe tail
%%
gen_unsafe_tl(Dst, [Arg]) -> hipe_tagscheme:unsafe_cdr(Dst, Arg).

%%
%% element
%%
gen_element(Dst, Args, IsGuard, Cont, Fail) ->
  Dst1 =
    case Dst of
      [] -> %% The result is not used.
	hipe_rtl:mk_new_var();
      [Dst0] -> Dst0
    end,
  [Index, Tuple] = Args,
  gen_element_1(Dst1, Index, Tuple, IsGuard, Cont, Fail, unknown, unknown).

gen_element_1(Dst, Index, Tuple, IsGuard, Cont, Fail, TupleInfo, IndexInfo) ->
  {FailLblName, FailCode} = gen_fail_code(Fail, badarg, IsGuard),
  [hipe_tagscheme:element(Dst, Index, Tuple, FailLblName, TupleInfo, IndexInfo),
   hipe_rtl:mk_goto(Cont),
   FailCode].

%%
%% unsafe element
%%
gen_unsafe_element(Dst, Index, Tuple) ->
  case hipe_rtl:is_imm(Index) of
    true -> hipe_tagscheme:unsafe_constant_element(Dst, Index, Tuple);
    false -> ?EXIT({illegal_index_to_unsafe_element,Index})
  end.

gen_unsafe_update_element(Tuple, Index, Value) ->
  case hipe_rtl:is_imm(Index) of
    true -> 
      hipe_tagscheme:unsafe_update_element(Tuple, Index, Value);
    false ->
      ?EXIT({illegal_index_to_unsafe_update_element,Index})
  end.


gen_closure_element(Dst, Index, Closure) ->
  hipe_tagscheme:unsafe_closure_element(Dst, Index, Closure).

%%
%% @doc Generate RTL code that writes a tuple header.
%%
gen_tuple_header(Ptr, Arity) ->
  Header = hipe_tagscheme:mk_arityval(Arity),
  hipe_rtl:mk_store(Ptr, hipe_rtl:mk_imm(0), hipe_rtl:mk_imm(Header)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Receives

gen_check_get_msg(Dsts, GotoCont, Fail) ->
  gen_check_get_msg_outofline(Dsts, GotoCont, Fail).

gen_clear_timeout([], GotoCont) ->
  case ?ERTS_IS_SMP of
    0 -> gen_clear_timeout_notsmp(GotoCont);
    1 -> gen_clear_timeout_smp(GotoCont)
  end.

-ifdef(notdef).	% for reference, currently unused
%%% check_get_msg is:
%%%	if (!PEEK_MESSAGE(p)) goto Fail;
%%%	Dst = ERL_MESSAGE_TERM(PEEK_MESSAGE(p));
%%% i.e.,
%%%	ErlMessage **save = p->msg.save;
%%%	ErlMessage *msg = *save;
%%%	if (!msg) goto Fail;
%%%	Dst = msg->m[0];
gen_check_get_msg_inline(Dsts, GotoCont, Fail) ->
  Save = hipe_rtl:mk_new_reg(),
  Msg = hipe_rtl:mk_new_reg(),
  TrueLbl = hipe_rtl:mk_new_label(),
  [load_p_field(Save, ?P_MSG_SAVE),
   load_struct_field(Msg, Save, 0),
   hipe_rtl:mk_branch(Msg, eq, hipe_rtl:mk_imm(0), Fail,
		      hipe_rtl:label_name(TrueLbl), 0.1),
   TrueLbl |
   case Dsts of
     [Dst] ->
       [load_struct_field(Dst, Msg, ?MSG_MESSAGE),
	GotoCont];
     [] -> % receive which throws away the message
       [GotoCont]
   end].
-endif.

%%% next_msg is:
%%%	SAVE_MESSAGE(p);
%%% i.e.,
%%%	ErlMessage **save = p->msg.save;
%%%	ErlMessage *msg = *save;
%%%	ErlMessage **next = &msg->next;
%%%	p->msg.save = next;
gen_next_msg([], GotoCont) ->
  Save = hipe_rtl:mk_new_reg(),
  Msg = hipe_rtl:mk_new_reg(),
  Next = hipe_rtl:mk_new_reg(),
  [load_p_field(Save, ?P_MSG_SAVE),
   load_struct_field(Msg, Save, 0),
   hipe_rtl:mk_alu(Next, Msg, 'add', hipe_rtl:mk_imm(?MSG_NEXT)),
   store_p_field(Next, ?P_MSG_SAVE),
   GotoCont].

%%% clear_timeout is:
%%%	p->flags &= ~F_TIMO; JOIN_MESSAGE(p);
%%% i.e.,
%%%	p->flags &= ~F_TIMO;
%%%	p->msg.save = &p->msg.first;
gen_clear_timeout_notsmp(GotoCont) ->
  Flags1 = hipe_rtl:mk_new_reg(),
  Flags2 = hipe_rtl:mk_new_reg_gcsafe(),
  First = hipe_rtl:mk_new_reg_gcsafe(),
  [load_p_field(Flags1, ?P_FLAGS),
   hipe_rtl:mk_alu(Flags2, Flags1, 'and', hipe_rtl:mk_imm(bnot(?F_TIMO))),
   store_p_field(Flags2, ?P_FLAGS),
   hipe_rtl_arch:pcb_address(First, ?P_MSG_FIRST),
   store_p_field(First, ?P_MSG_SAVE),
   GotoCont].

gen_check_get_msg_outofline(Dsts, GotoCont, Fail) ->
  RetLbl = hipe_rtl:mk_new_label(),
  TrueLbl = hipe_rtl:mk_new_label(),
  Tmp = hipe_rtl:mk_new_reg(),
  TheNonValue = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  [hipe_rtl_arch:call_bif([Tmp], check_get_msg, [],
			  hipe_rtl:label_name(RetLbl), []),
   RetLbl,
   hipe_rtl:mk_branch(Tmp, eq, TheNonValue, Fail,
		      hipe_rtl:label_name(TrueLbl), 0.1),
   TrueLbl |
   case Dsts of
     [Dst] ->
       [hipe_rtl:mk_move(Dst, Tmp),
	GotoCont];
     [] -> % receive which throws away the message
       [GotoCont]
   end].

gen_clear_timeout_smp(GotoCont) ->
  RetLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl_arch:call_bif([], clear_timeout, [],
			  hipe_rtl:label_name(RetLbl), []),
   RetLbl,
   GotoCont].

gen_select_msg([], Cont) ->
  [hipe_rtl_arch:call_bif([], select_msg, [], Cont, [])].

gen_suspend_msg([], Cont) ->
  [hipe_rtl:mk_call([], suspend_msg, [], Cont, [], not_remote)].

%% --------------------------------------------------------------------
%%
%% Floating point handling 
%%

gen_fclearerror() ->
  case ?P_FP_EXCEPTION of
    [] ->
      [];
    Offset ->
      Tmp = hipe_rtl:mk_new_reg(),
      FailLbl = hipe_rtl:mk_new_label(),
      ContLbl = hipe_rtl:mk_new_label(),
      ContLblName = hipe_rtl:label_name(ContLbl),
      [hipe_rtl_arch:pcb_load(Tmp, Offset),
       hipe_rtl:mk_branch(Tmp, eq, hipe_rtl:mk_imm(0), ContLblName,
			  hipe_rtl:label_name(FailLbl), 0.9),
       FailLbl,
       hipe_rtl:mk_call([], 'fclearerror_error', [], [], [], not_remote),
       hipe_rtl:mk_goto(ContLblName),
       ContLbl]
  end.

gen_fcheckerror(ContLbl, FailLbl) ->
  case ?P_FP_EXCEPTION of
    [] ->
      [];
    Offset ->
      Tmp = hipe_rtl:mk_new_reg(),
      TmpFailLbl0 = hipe_rtl:mk_new_label(),
      FailCode = fp_fail_code(TmpFailLbl0, FailLbl),
      PreFailLbl = hipe_rtl:mk_new_label(),
      hipe_rtl_arch:fwait() ++
	[hipe_rtl_arch:pcb_load(Tmp, Offset),
	 hipe_rtl:mk_branch(Tmp, eq, hipe_rtl:mk_imm(0), ContLbl,
			    hipe_rtl:label_name(PreFailLbl), 0.9),
	 PreFailLbl,
	 hipe_rtl_arch:pcb_store(Offset, hipe_rtl:mk_imm(0)),
	 hipe_rtl:mk_goto(hipe_rtl:label_name(TmpFailLbl0)) |
	 FailCode]
  end.

gen_conv_to_float(Dst, [Src], ContLbl, FailLbl) ->
  case hipe_rtl:is_var(Src) of
    true ->
      Tmp = hipe_rtl:mk_new_var(),
      TmpReg = hipe_rtl:mk_new_reg_gcsafe(),
      TrueFixNum = hipe_rtl:mk_new_label(),
      ContFixNum = hipe_rtl:mk_new_label(),
      TrueFp = hipe_rtl:mk_new_label(),
      ContFp = hipe_rtl:mk_new_label(),
      ContBigNum = hipe_rtl:mk_new_label(),
      TestFixNum = hipe_tagscheme:test_fixnum(Src,
					      hipe_rtl:label_name(TrueFixNum),
					      hipe_rtl:label_name(ContFixNum),
					      0.5),
      TestFp = hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(TrueFp),
					  hipe_rtl:label_name(ContFp), 0.5),
      GotoCont = hipe_rtl:mk_goto(ContLbl),
      TmpFailLbl0 = hipe_rtl:mk_new_label(),
      FailCode = fp_fail_code(TmpFailLbl0, FailLbl),

      TestFixNum ++
	[TrueFixNum, 
    	 hipe_tagscheme:untag_fixnum(TmpReg, Src),
	 hipe_rtl:mk_fconv(Dst, TmpReg),
	 GotoCont,
	 ContFixNum] ++ 
	TestFp ++
	[TrueFp, 
	 hipe_tagscheme:unsafe_untag_float(Dst, Src), 
	 GotoCont, 
	 ContFp] ++
	[hipe_rtl:mk_call([Tmp], conv_big_to_float, [Src],
			  hipe_rtl:label_name(ContBigNum),
			  hipe_rtl:label_name(TmpFailLbl0), not_remote)]++
	FailCode ++
	[ContBigNum,
	 hipe_tagscheme:unsafe_untag_float(Dst, Tmp)];
    _ ->
      %% This must be an attempt to convert an illegal term.
      [gen_fail_code(FailLbl, badarith)]
  end.

