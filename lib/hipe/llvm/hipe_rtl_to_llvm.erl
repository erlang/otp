%% -*- erlang-indent-level: 2 -*-

-module(hipe_rtl_to_llvm).
-author("Chris Stavrakakis, Yiannis Tsiouris").

-export([translate/2]).    % the main function of this module
-export([fix_mfa_name/1]). % a help function used in hipe_llvm_main

-include("../rtl/hipe_rtl.hrl").
-include("../rtl/hipe_literals.hrl").
-include("hipe_llvm_arch.hrl").

-define(WORD_WIDTH, (?bytes_to_bits(hipe_rtl_arch:word_size()))).
-define(BRANCH_META_TAKEN, "0").
-define(BRANCH_META_NOT_TAKEN, "1").
-define(FIRST_FREE_META_NO, 2).
-define(HIPE_LITERALS_META, "hipe.literals").

%%------------------------------------------------------------------------------
%% @doc Main function for translating an RTL function to LLVM Assembly. Takes as
%%      input the RTL code and the variable indexes of possible garbage
%%      collection roots and returns the corresponing LLVM, a dictionary with
%%      all the relocations in the code and a hipe_consttab() with informaton
%%      about data.
%%------------------------------------------------------------------------------
translate(RTL, Roots) ->
  Fun = hipe_rtl:rtl_fun(RTL),
  Params = hipe_rtl:rtl_params(RTL),
  Data = hipe_rtl:rtl_data(RTL),
  Code = hipe_rtl:rtl_code(RTL),
  %% Init unique symbol generator and initialize the label counter to the last
  %% RTL label.
  hipe_gensym:init(llvm),
  {_, MaxLabel} = hipe_rtl:rtl_label_range(RTL),
  put({llvm,label_count}, MaxLabel + 1),
  %% Put first label of RTL code in process dictionary
  find_code_entry_label(Code),
  %% Initialize relocations symbol dictionary
  Relocs = dict:new(),
  %% Print RTL to file
  %% {ok, File_rtl} = file:open("rtl_" ++integer_to_list(random:uniform(2000))
  %%                            ++ ".rtl", [write]),
  %% hipe_rtl:pp(File_rtl, RTL),
  %% file:close(File_rtl),

  %% Pass on RTL code to handle exception handling and identify labels of Fail
  %% Blocks
  {Code1, FailLabels} = fix_code(Code),
  %% Allocate stack slots for each virtual register and declare gc roots
  AllocaStackCode = alloca_stack(Code1, Params, Roots),
  %% Translate Code
  {LLVM_Code1, Relocs1, NewData} =
    translate_instr_list(Code1, [], Relocs, Data),
  %% Create LLVM code to declare relocation symbols as external symbols along
  %% with local variables in order to use them as just any other variable
  {FinalRelocs, ExternalDecl0, LocalVars} =
    handle_relocations(Relocs1, Data, Fun),
  ExternalDecl = add_literals_metadata(ExternalDecl0),
  %% Pass on LLVM code in order to create Fail blocks and a landingpad
  %% instruction to each one
  LLVM_Code2 = add_landingpads(LLVM_Code1, FailLabels),
  %% Create LLVM Code for the compiled function
  LLVM_Code3 = create_function_definition(Fun, Params, LLVM_Code2,
                                          AllocaStackCode ++ LocalVars),
  %% Final Code = CompiledFunction + External Declarations
  FinalLLVMCode = [LLVM_Code3 | ExternalDecl],
  {FinalLLVMCode, FinalRelocs, NewData}.

find_code_entry_label([]) ->
  exit({?MODULE, find_code_entry_label, "Empty code"});
find_code_entry_label([I|_]) ->
  case hipe_rtl:is_label(I) of
    true ->
      put(first_label, hipe_rtl:label_name(I));
    false ->
      exit({?MODULE, find_code_entry_label, "First instruction is not a label"})
  end.

%% @doc Create a stack slot for each virtual register. The stack slots
%%      that correspond to possible garbage collection roots must be
%%      marked as such.
alloca_stack(Code, Params, Roots) ->
  %% Find all assigned virtual registers
  Destinations = collect_destinations(Code),
  %% Declare virtual registers, and declare garbage collection roots
  do_alloca_stack(Destinations++Params, Params, Roots).

collect_destinations(Code) ->
  lists:usort(lists:flatmap(fun insn_dst/1, Code)).

do_alloca_stack(Destinations, Params, Roots) ->
  do_alloca_stack(Destinations, Params, Roots, []).

do_alloca_stack([], _, _, Acc) ->
  Acc;
do_alloca_stack([D|Ds], Params, Roots, Acc) ->
  {Name, _I} = trans_dst(D),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  ByteTyPtr = hipe_llvm:mk_pointer(hipe_llvm:mk_int(8)),
  case hipe_rtl:is_var(D) of
    true ->
      Num = hipe_rtl:var_index(D),
      I1 = hipe_llvm:mk_alloca(Name, WordTy, [], []),
      case lists:member(Num, Roots) of
        true -> %% Variable is a possible Root
          T1 = mk_temp(),
          BYTE_TYPE_PP = hipe_llvm:mk_pointer(ByteTyPtr),
          I2 =
            hipe_llvm:mk_conversion(T1, bitcast, WordTyPtr, Name, BYTE_TYPE_PP),
          GcRootArgs = [{BYTE_TYPE_PP, T1}, {ByteTyPtr, "@gc_metadata"}],
          I3 = hipe_llvm:mk_call([], false, [], [], hipe_llvm:mk_void(),
                                 "@llvm.gcroot", GcRootArgs, []),
          I4 = case lists:member(D, Params) of
                 false ->
		   hipe_llvm:mk_store(WordTy, "-5", WordTyPtr, Name,
				      [], [], false);
                 true -> []
               end,
          do_alloca_stack(Ds, Params, Roots, [I1, I2, I3, I4 | Acc]);
        false ->
          do_alloca_stack(Ds, Params, Roots, [I1|Acc])
      end;
    false ->
      case hipe_rtl:is_reg(D) andalso isPrecoloured(D) of
        true -> %% Precoloured registers are mapped to "special" stack slots
          do_alloca_stack(Ds, Params, Roots,  Acc);
        false ->
          I1 = case hipe_rtl:is_fpreg(D) of
		 true ->
		   FloatTy = hipe_llvm:mk_double(),
		   hipe_llvm:mk_alloca(Name, FloatTy, [], []);
		 false -> hipe_llvm:mk_alloca(Name, WordTy, [], [])
	       end,
	  do_alloca_stack(Ds, Params, Roots, [I1|Acc])
      end
  end.

%%------------------------------------------------------------------------------
%% @doc Translation of the linearized RTL Code. Each RTL instruction is
%%      translated to a list of LLVM Assembly instructions. The relocation
%%      dictionary is updated when needed.
%%------------------------------------------------------------------------------
translate_instr_list([], Acc, Relocs, Data) ->
  {lists:reverse(lists:flatten(Acc)), Relocs, Data};
translate_instr_list([I | Is], Acc, Relocs, Data) ->
  {Acc1, NewRelocs, NewData} = translate_instr(I, Relocs, Data),
  translate_instr_list(Is, [Acc1 | Acc], NewRelocs, NewData).

translate_instr(I, Relocs, Data) ->
  case I of
    #alu{} ->
      {I2, Relocs2} = trans_alu(I, Relocs),
      {I2, Relocs2, Data};
    #alub{} ->
      {I2, Relocs2} = trans_alub(I, Relocs),
      {I2, Relocs2, Data};
    #branch{} ->
      {I2, Relocs2} = trans_branch(I, Relocs),
      {I2, Relocs2, Data};
    #call{} ->
      {I2, Relocs2} =
        case hipe_rtl:call_fun(I) of
          %% In AMD64 this instruction does nothing!
          %% TODO: chech use of fwait in other architectures!
          fwait ->
            {[], Relocs};
          _ ->
            trans_call(I, Relocs)
        end,
      {I2, Relocs2, Data};
    #comment{} ->
      {I2, Relocs2} = trans_comment(I, Relocs),
      {I2, Relocs2, Data};
    #enter{} ->
      {I2, Relocs2} = trans_enter(I, Relocs),
      {I2, Relocs2, Data};
    #fconv{} ->
      {I2, Relocs2} = trans_fconv(I, Relocs),
      {I2, Relocs2, Data};
    #fload{} ->
      {I2, Relocs2} = trans_fload(I, Relocs),
      {I2, Relocs2, Data};
    #fmove{} ->
      {I2, Relocs2} = trans_fmove(I, Relocs),
      {I2, Relocs2, Data};
    #fp{} ->
      {I2, Relocs2} = trans_fp(I, Relocs),
      {I2, Relocs2, Data};
    #fp_unop{} ->
      {I2, Relocs2} = trans_fp_unop(I, Relocs),
      {I2, Relocs2, Data};
    #fstore{} ->
      {I2, Relocs2} = trans_fstore(I, Relocs),
      {I2, Relocs2, Data};
    #goto{} ->
      {I2, Relocs2} = trans_goto(I, Relocs),
      {I2, Relocs2, Data};
    #label{} ->
      {I2, Relocs2} = trans_label(I, Relocs),
      {I2, Relocs2, Data};
    #load{} ->
      {I2, Relocs2} = trans_load(I, Relocs),
      {I2, Relocs2, Data};
    #load_address{} ->
      {I2, Relocs2} = trans_load_address(I, Relocs),
      {I2, Relocs2, Data};
    #load_atom{} ->
      {I2, Relocs2} = trans_load_atom(I, Relocs),
      {I2, Relocs2, Data};
    #move{} ->
      {I2, Relocs2} = trans_move(I, Relocs),
      {I2, Relocs2, Data};
    #return{} ->
      {I2, Relocs2} = trans_return(I, Relocs),
      {I2, Relocs2, Data};
    #store{} ->
      {I2, Relocs2} = trans_store(I, Relocs),
      {I2, Relocs2, Data};
    #switch{} -> %% Only switch instruction updates Data
      {I2, Relocs2, NewData} = trans_switch(I, Relocs, Data),
      {I2, Relocs2, NewData};
    Other ->
      exit({?MODULE, translate_instr, {"Unknown RTL instruction", Other}})
  end.

%%
%% alu
%%
trans_alu(I, Relocs) ->
  RtlDst = hipe_rtl:alu_dst(I),
  TmpDst = mk_temp(),
  {Src1, I1} = trans_src(hipe_rtl:alu_src1(I)),
  {Src2, I2} = trans_src(hipe_rtl:alu_src2(I)),
  Op = trans_op(hipe_rtl:alu_op(I)),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  I3 = hipe_llvm:mk_operation(TmpDst, Op, WordTy, Src1, Src2, []),
  I4 = store_stack_dst(TmpDst, RtlDst),
  {[I4, I3, I2, I1], Relocs}.

%%
%% alub
%%
trans_alub(I, Relocs) ->
  case hipe_rtl:alub_cond(I) of
    Op when Op =:= overflow orelse Op =:= not_overflow ->
      trans_alub_overflow(I, signed, Relocs);
    ltu -> %% ltu means unsigned overflow
      trans_alub_overflow(I, unsigned, Relocs);
    _ ->
      trans_alub_no_overflow(I, Relocs)
  end.

trans_alub_overflow(I, Sign, Relocs) ->
  {Src1, I1} = trans_src(hipe_rtl:alub_src1(I)),
  {Src2, I2} = trans_src(hipe_rtl:alub_src2(I)),
  RtlDst = hipe_rtl:alub_dst(I),
  TmpDst = mk_temp(),
  Name = trans_alub_op(I, Sign),
  NewRelocs = relocs_store(Name, {call, {llvm, Name, 2}}, Relocs),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  ReturnType = hipe_llvm:mk_struct([WordTy, hipe_llvm:mk_int(1)]),
  T1 = mk_temp(),
  I3 = hipe_llvm:mk_call(T1, false, [], [], ReturnType, "@" ++ Name,
			                   [{WordTy, Src1}, {WordTy, Src2}], []),
  %% T1{0}: result of the operation
  I4 = hipe_llvm:mk_extractvalue(TmpDst, ReturnType, T1 , "0", []),
  I5 = store_stack_dst(TmpDst, RtlDst),
  T2 = mk_temp(),
  %% T1{1}: Boolean variable indicating overflow
  I6 = hipe_llvm:mk_extractvalue(T2, ReturnType, T1, "1", []),
  {TrueLabel, FalseLabel, MetaData} =
    case hipe_rtl:alub_cond(I) of
      Op when Op =:= overflow orelse Op =:= ltu ->
	{mk_jump_label(hipe_rtl:alub_true_label(I)),
	 mk_jump_label(hipe_rtl:alub_false_label(I)),
	 branch_metadata(hipe_rtl:alub_pred(I))};
      not_overflow ->
	{mk_jump_label(hipe_rtl:alub_false_label(I)),
	 mk_jump_label(hipe_rtl:alub_true_label(I)),
	 branch_metadata(1 - hipe_rtl:alub_pred(I))}
    end,
  I7 = hipe_llvm:mk_br_cond(T2, TrueLabel, FalseLabel, MetaData),
  {[I7, I6, I5, I4, I3, I2, I1], NewRelocs}.

trans_alub_op(I, Sign) ->
  Name =
    case Sign of
      signed ->
        case hipe_rtl:alub_op(I) of
          add -> "llvm.sadd.with.overflow.";
          mul -> "llvm.smul.with.overflow.";
          sub -> "llvm.ssub.with.overflow.";
          Op  -> exit({?MODULE, trans_alub_op, {"Unknown alub operator", Op}})
        end;
      unsigned ->
        case hipe_rtl:alub_op(I) of
          add -> "llvm.uadd.with.overflow.";
          mul -> "llvm.umul.with.overflow.";
          sub -> "llvm.usub.with.overflow.";
          Op  -> exit({?MODULE, trans_alub_op, {"Unknown alub operator", Op}})
        end
    end,
  Type =
    case hipe_rtl_arch:word_size() of
      4 -> "i32";
      8 -> "i64"
      %% Other -> exit({?MODULE, trans_alub_op, {"Unknown type", Other}})
    end,
  Name ++ Type.

trans_alub_no_overflow(I, Relocs) ->
  %% alu
  T = hipe_rtl:mk_alu(hipe_rtl:alub_dst(I), hipe_rtl:alub_src1(I),
                      hipe_rtl:alub_op(I), hipe_rtl:alub_src2(I)),
  %% A trans_alu instruction cannot change relocations
  {I1, _} = trans_alu(T, Relocs),
  %% icmp
  %% Translate destination as src, to match with the semantics of instruction
  {Dst, I2} = trans_src(hipe_rtl:alub_dst(I)),
  Cond = trans_rel_op(hipe_rtl:alub_cond(I)),
  T3 = mk_temp(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  I5 = hipe_llvm:mk_icmp(T3, Cond, WordTy, Dst, "0"),
  %% br
  Metadata = branch_metadata(hipe_rtl:alub_pred(I)),
  True_label = mk_jump_label(hipe_rtl:alub_true_label(I)),
  False_label = mk_jump_label(hipe_rtl:alub_false_label(I)),
  I6 = hipe_llvm:mk_br_cond(T3, True_label, False_label, Metadata),
  {[I6, I5, I2, I1], Relocs}.

%%
%% branch
%%
trans_branch(I, Relocs) ->
  {Src1, I1} = trans_src(hipe_rtl:branch_src1(I)),
  {Src2, I2} = trans_src(hipe_rtl:branch_src2(I)),
  Cond = trans_rel_op(hipe_rtl:branch_cond(I)),
  %% icmp
  T1 = mk_temp(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  I3 = hipe_llvm:mk_icmp(T1, Cond, WordTy, Src1, Src2),
  %% br
  True_label = mk_jump_label(hipe_rtl:branch_true_label(I)),
  False_label = mk_jump_label(hipe_rtl:branch_false_label(I)),
  Metadata = branch_metadata(hipe_rtl:branch_pred(I)),
  I4 = hipe_llvm:mk_br_cond(T1, True_label, False_label, Metadata),
  {[I4, I3, I2, I1], Relocs}.

branch_metadata(X) when X =:= 0.5 -> [];
branch_metadata(X) when X > 0.5 -> ?BRANCH_META_TAKEN;
branch_metadata(X) when X < 0.5 -> ?BRANCH_META_NOT_TAKEN.

%%
%% call
%%
trans_call(I, Relocs) ->
  RtlCallArgList= hipe_rtl:call_arglist(I),
  RtlCallName = hipe_rtl:call_fun(I),
  {I0, Relocs1} = expose_closure(RtlCallName, RtlCallArgList, Relocs),
  TmpDst = mk_temp(),
  {CallArgs, I1} = trans_call_args(RtlCallArgList),
  FixedRegs = fixed_registers(),
  {LoadedFixedRegs, I2} = load_fixed_regs(FixedRegs),
  FinalArgs = fix_reg_args(LoadedFixedRegs) ++ CallArgs,
  {Name, I3, Relocs2} =
    trans_call_name(RtlCallName, Relocs1, CallArgs, FinalArgs),
  T1 = mk_temp(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  FunRetTy = hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
  I4 =
    case hipe_rtl:call_fail(I) of
      %% Normal Call
      [] ->
        hipe_llvm:mk_call(T1, false, "cc 11", [], FunRetTy, Name, FinalArgs,
                          []);
      %% Call With Exception
      FailLabelNum ->
        TrueLabel = "L" ++ integer_to_list(hipe_rtl:call_normal(I)),
        FailLabel = "%FL" ++ integer_to_list(FailLabelNum),
        II1 =
          hipe_llvm:mk_invoke(T1, "cc 11", [], FunRetTy, Name, FinalArgs, [],
                              "%" ++ TrueLabel, FailLabel),
        II2 = hipe_llvm:mk_label(TrueLabel),
        [II2, II1]
    end,
  I5 = store_fixed_regs(FixedRegs, T1),
  I6 =
    case hipe_rtl:call_dstlist(I) of
      [] -> []; %% No return value
      [Destination] ->
        II3 =
          hipe_llvm:mk_extractvalue(TmpDst, FunRetTy, T1,
                                    integer_to_list(?NR_PINNED_REGS), []),
        II4 = store_stack_dst(TmpDst, Destination),
        [II4, II3]
    end,
  I7 =
    case hipe_rtl:call_continuation(I) of
      [] -> []; %% No continuation
      CC ->
        {II5, _} = trans_goto(hipe_rtl:mk_goto(CC), Relocs2),
        II5
    end,
  {[I7, I6, I5, I4, I3, I2, I1, I0], Relocs2}.

%% In case of call to a register (closure call) with more than ?NR_ARG_REGS
%% arguments we must track the offset this call in the code, in order to
%% to correct the stack descriptor. So, we insert a new Label and add this label
%% to the "table_closures"
%% --------------------------------|--------------------------------------------
%%        Old Code                 |           New Code
%% --------------------------------|--------------------------------------------
%%                                 |           br %ClosureLabel
%%        call %reg(Args)          |           ClosureLabel:
%%                                 |           call %reg(Args)
expose_closure(CallName, CallArgs, Relocs) ->
  CallArgsNr = length(CallArgs),
  case hipe_rtl:is_reg(CallName) andalso CallArgsNr > ?NR_ARG_REGS of
    true ->
      LabelNum = hipe_gensym:new_label(llvm),
      ClosureLabel = hipe_llvm:mk_label(mk_label(LabelNum)),
      JumpIns = hipe_llvm:mk_br(mk_jump_label(LabelNum)),
      Relocs1 =
        relocs_store({CallName, LabelNum},
                     {closure_label, LabelNum, CallArgsNr - ?NR_ARG_REGS},
                     Relocs),
      {[ClosureLabel, JumpIns], Relocs1};
    false ->
      {[], Relocs}
  end.

trans_call_name(RtlCallName, Relocs, CallArgs, FinalArgs) ->
  case RtlCallName of
    PrimOp when is_atom(PrimOp) ->
      LlvmName = trans_prim_op(PrimOp),
      Relocs1 =
        relocs_store(LlvmName, {call, {bif, PrimOp, length(CallArgs)}}, Relocs),
      {"@" ++ LlvmName, [], Relocs1};
    {M, F, A} when is_atom(M), is_atom(F), is_integer(A) ->
      LlvmName = trans_mfa_name({M, F, A}),
      Relocs1 =
        relocs_store(LlvmName, {call, {M, F, length(CallArgs)}}, Relocs),
      {"@" ++ LlvmName, [], Relocs1};
    Reg ->
      case hipe_rtl:is_reg(Reg) of
        true ->
	  %% In case of a closure call, the register holding the address
	  %% of the closure must be converted to function type in
	  %% order to make the call
          TT1 = mk_temp(),
          {RegName, II1} = trans_src(Reg),
          WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
          WordTyPtr = hipe_llvm:mk_pointer(WordTy),
          II2 =
            hipe_llvm:mk_conversion(TT1, inttoptr, WordTy, RegName, WordTyPtr),
          TT2 = mk_temp(),
          ArgsTypeList = lists:duplicate(length(FinalArgs), WordTy),
          FunRetTy =
            hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
          FunType = hipe_llvm:mk_fun(FunRetTy, ArgsTypeList),
          FunTypeP = hipe_llvm:mk_pointer(FunType),
          II3 = hipe_llvm:mk_conversion(TT2, bitcast, WordTyPtr, TT1, FunTypeP),
          {TT2, [II3, II2, II1], Relocs};
        false ->
          exit({?MODULE, trans_call, {"Unimplemented call to", RtlCallName}})
      end
  end.

%%
trans_call_args(ArgList) ->
  {Args, I} = lists:unzip(trans_args(ArgList)),
  %% Reverse arguments that are passed to stack to match with the Erlang
  %% calling convention. (Propably not needed in prim calls.)
  ReversedArgs =
    case erlang:length(Args) > ?NR_ARG_REGS of
      false ->
	Args;
      true ->
	{ArgsInRegs, ArgsInStack} = lists:split(?NR_ARG_REGS, Args),
	ArgsInRegs ++ lists:reverse(ArgsInStack)
    end,
  %% Reverse I, because some of the arguments may go out of scope and
  %% should be killed(store -5). When two or more arguments are they
  %% same, then order matters!
  {ReversedArgs, lists:reverse(I)}.

%%
%% trans_comment
%%
trans_comment(I, Relocs) ->
  I1 = hipe_llvm:mk_comment(hipe_rtl:comment_text(I)),
  {I1, Relocs}.

%%
%% enter
%%
trans_enter(I, Relocs) ->
  {CallArgs, I0} = trans_call_args(hipe_rtl:enter_arglist(I)),
  FixedRegs = fixed_registers(),
  {LoadedFixedRegs, I1} = load_fixed_regs(FixedRegs),
  FinalArgs = fix_reg_args(LoadedFixedRegs) ++ CallArgs,
  {Name, I2, NewRelocs} =
    trans_call_name(hipe_rtl:enter_fun(I), Relocs, CallArgs, FinalArgs),
  T1 = mk_temp(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  FunRetTy = hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
  I3 = hipe_llvm:mk_call(T1, true, "cc 11", [], FunRetTy, Name, FinalArgs, []),
  I4 = hipe_llvm:mk_ret([{FunRetTy, T1}]),
  {[I4, I3, I2, I1, I0], NewRelocs}.

%%
%% fconv
%%
trans_fconv(I, Relocs) ->
  %% XXX: Can a fconv destination be a precoloured reg?
  RtlDst = hipe_rtl:fconv_dst(I),
  TmpDst = mk_temp(),
  {Src, I1} =  trans_float_src(hipe_rtl:fconv_src(I)),
  FloatTy = hipe_llvm:mk_double(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  I2 = hipe_llvm:mk_conversion(TmpDst, sitofp, WordTy, Src, FloatTy),
  I3 = store_float_stack(TmpDst, RtlDst),
  {[I3, I2, I1], Relocs}.


%% TODO: fload, fstore, fmove, and fp are almost the same with load, store, move
%% and alu. Maybe we should join them.

%%
%% fload
%%
trans_fload(I, Relocs) ->
  RtlDst = hipe_rtl:fload_dst(I),
  RtlSrc = hipe_rtl:fload_src(I),
  _Offset = hipe_rtl:fload_offset(I),
  TmpDst = mk_temp(),
  {Src, I1} = trans_float_src(RtlSrc),
  {Offset, I2} = trans_float_src(_Offset),
  T1 = mk_temp(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  FloatTyPtr = hipe_llvm:mk_pointer(hipe_llvm:mk_double()),
  I3 = hipe_llvm:mk_operation(T1, add, WordTy, Src, Offset, []),
  T2 = mk_temp(),
  I4 = hipe_llvm:mk_conversion(T2, inttoptr,  WordTy, T1, FloatTyPtr),
  I5 = hipe_llvm:mk_load(TmpDst, FloatTyPtr, T2, [], [], false),
  I6 = store_float_stack(TmpDst, RtlDst),
  {[I6, I5, I4, I3, I2, I1], Relocs}.

%%
%% fmove
%%
trans_fmove(I, Relocs) ->
  RtlDst = hipe_rtl:fmove_dst(I),
  RtlSrc = hipe_rtl:fmove_src(I),
  {Src, I1} = trans_float_src(RtlSrc),
  I2 = store_float_stack(Src, RtlDst),
  {[I2, I1], Relocs}.

%%
%% fp
%%
trans_fp(I, Relocs) ->
  %% XXX: Just copied trans_alu...think again..
  RtlDst = hipe_rtl:fp_dst(I),
  RtlSrc1 = hipe_rtl:fp_src1(I),
  RtlSrc2 = hipe_rtl:fp_src2(I),
  %% Destination cannot be a precoloured register
  FloatTy = hipe_llvm:mk_double(),
  FloatTyPtr = hipe_llvm:mk_pointer(FloatTy),
  TmpDst = mk_temp(),
  {Src1, I1} = trans_float_src(RtlSrc1),
  {Src2, I2} = trans_float_src(RtlSrc2),
  Op = trans_fp_op(hipe_rtl:fp_op(I)),
  I3 = hipe_llvm:mk_operation(TmpDst, Op, FloatTy, Src1, Src2, []),
  I4 = store_float_stack(TmpDst, RtlDst),
  %% Synchronization for floating point exceptions
  I5 = hipe_llvm:mk_store(FloatTy, TmpDst, FloatTyPtr, "%exception_sync", [],
                          [], true),
  T1 = mk_temp(),
  I6 = hipe_llvm:mk_load(T1, FloatTyPtr, "%exception_sync", [], [], true),
  {[I6, I5, I4, I3, I2, I1], Relocs}.

%%
%% fp_unop
%%
trans_fp_unop(I, Relocs) ->
  RtlDst = hipe_rtl:fp_unop_dst(I),
  RtlSrc = hipe_rtl:fp_unop_src(I),
  %% Destination cannot be a precoloured register
  TmpDst = mk_temp(),
  {Src, I1} = trans_float_src(RtlSrc),
  Op =  trans_fp_op(hipe_rtl:fp_unop_op(I)),
  FloatTy = hipe_llvm:mk_double(),
  I2 = hipe_llvm:mk_operation(TmpDst, Op, FloatTy, "0.0", Src, []),
  I3 = store_float_stack(TmpDst, RtlDst),
  {[I3, I2, I1], Relocs}.
%% TODO: Fix fp_unop in a way like the following. You must change trans_dest,
%% in order to call float_to_list in a case of float constant. Maybe the type
%% check is expensive...
%% Dst = hipe_rtl:fp_unop_dst(I),
%% Src = hipe_rtl:fp_unop_src(I),
%% Op = hipe_rtl:fp_unop_op(I),
%% Zero = hipe_rtl:mk_imm(0.0),
%% I1 = hipe_rtl:mk_fp(Dst, Zero, Op, Src),
%% trans_fp(I, Relocs1).

%%
%% fstore
%%
trans_fstore(I, Relocs) ->
  Base = hipe_rtl:fstore_base(I),
  case isPrecoloured(Base) of
    true ->
      trans_fstore_reg(I, Relocs);
    false ->
      exit({?MODULE, trans_fstore ,{"Not implemented yet", false}})
  end.

trans_fstore_reg(I, Relocs) ->
  {Base, I0}  = trans_reg(hipe_rtl:fstore_base(I), dst),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  FloatTy = hipe_llvm:mk_double(),
  FloatTyPtr = hipe_llvm:mk_pointer(FloatTy),
  T1 = mk_temp(),
  I1 = hipe_llvm:mk_load(T1, WordTyPtr, Base, [],  [], false),
  {Offset, I2} = trans_src(hipe_rtl:fstore_offset(I)),
  T2 = mk_temp(),
  I3 = hipe_llvm:mk_operation(T2, add, WordTy, T1, Offset, []),
  T3 = mk_temp(),
  I4 = hipe_llvm:mk_conversion(T3, inttoptr, WordTy, T2, FloatTyPtr),
  {Value, I5} = trans_src(hipe_rtl:fstore_src(I)),
  I6 = hipe_llvm:mk_store(FloatTy, Value, FloatTyPtr, T3, [], [], false),
  {[I6, I5, I4, I3, I2, I1, I0], Relocs}.

%%
%% goto
%%
trans_goto(I, Relocs) ->
  I1 = hipe_llvm:mk_br(mk_jump_label(hipe_rtl:goto_label(I))),
  {I1, Relocs}.

%%
%% label
%%
trans_label(I, Relocs) ->
  Label = mk_label(hipe_rtl:label_name(I)),
  I1 = hipe_llvm:mk_label(Label),
  {I1, Relocs}.

%%
%% load
%%
trans_load(I, Relocs) ->
  RtlDst = hipe_rtl:load_dst(I),
  TmpDst = mk_temp(),
  %% XXX: Why translate them independently? ------------------------
  {Src, I1} = trans_src(hipe_rtl:load_src(I)),
  {Offset, I2} = trans_src(hipe_rtl:load_offset(I)),
  T1 = mk_temp(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  I3 = hipe_llvm:mk_operation(T1, add, WordTy, Src, Offset, []),
  %%----------------------------------------------------------------
  I4 = case hipe_rtl:load_size(I) of
         word ->
           T2 = mk_temp(),
           II1 = hipe_llvm:mk_conversion(T2, inttoptr, WordTy, T1, WordTyPtr),
           II2 = hipe_llvm:mk_load(TmpDst, WordTyPtr, T2, [], [], false),
           [II2, II1];
         Size ->
           LoadType = llvm_type_from_size(Size),
           LoadTypeP = hipe_llvm:mk_pointer(LoadType),
           T2 = mk_temp(),
           II1 = hipe_llvm:mk_conversion(T2, inttoptr, WordTy, T1, LoadTypeP),
           T3 = mk_temp(),
           LoadTypePointer = hipe_llvm:mk_pointer(LoadType),
           II2 = hipe_llvm:mk_load(T3, LoadTypePointer, T2, [], [], false),
           Conversion =
             case hipe_rtl:load_sign(I) of
               signed -> sext;
               unsigned -> zext
             end,
           II3 =
             hipe_llvm:mk_conversion(TmpDst, Conversion, LoadType, T3, WordTy),
           [II3, II2, II1]
       end,
  I5 = store_stack_dst(TmpDst, RtlDst),
  {[I5, I4, I3, I2, I1], Relocs}.

%%
%% load_address
%%
trans_load_address(I, Relocs) ->
  RtlDst = hipe_rtl:load_address_dst(I),
  RtlAddr = hipe_rtl:load_address_addr(I),
  {Addr, NewRelocs} =
    case hipe_rtl:load_address_type(I) of
      constant ->
        {"%DL" ++ integer_to_list(RtlAddr) ++ "_var", Relocs};
      closure  ->
        {{_, ClosureName, _}, _, _} = RtlAddr,
        FixedClosureName = fix_closure_name(ClosureName),
        Relocs1 = relocs_store(FixedClosureName, {closure, RtlAddr}, Relocs),
        {"%" ++ FixedClosureName ++ "_var", Relocs1};
      type ->
        exit({?MODULE, trans_load_address,
             {"Type not implemented in load_address", RtlAddr}})
    end,
  I1 = store_stack_dst(Addr, RtlDst),
  {[I1], NewRelocs}.

%%
%% load_atom
%%
trans_load_atom(I, Relocs) ->
  RtlDst = hipe_rtl:load_atom_dst(I),
  RtlAtom = hipe_rtl:load_atom_atom(I),
  AtomName = "atom_" ++ make_llvm_id(atom_to_list(RtlAtom)),
  AtomVar = "%" ++ AtomName ++ "_var",
  NewRelocs = relocs_store(AtomName, {atom, RtlAtom}, Relocs),
  I1 = store_stack_dst(AtomVar, RtlDst),
  {[I1], NewRelocs}.

%%
%% move
%%
trans_move(I, Relocs) ->
  RtlDst = hipe_rtl:move_dst(I),
  RtlSrc = hipe_rtl:move_src(I),
  {Src, I1} = trans_src(RtlSrc),
  I2 = store_stack_dst(Src, RtlDst),
  {[I2, I1], Relocs}.

%%
%% return
%%
trans_return(I, Relocs) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  {VarRet, I1} =
    case hipe_rtl:return_varlist(I) of
      [] ->
	{[], []};
      [A] ->
	{Name, II1} = trans_src(A),
	{[{WordTy, Name}], II1}
    end,
  FixedRegs = fixed_registers(),
  {LoadedFixedRegs, I2} = load_fixed_regs(FixedRegs),
  FixedRet = [{WordTy, X} || X <- LoadedFixedRegs],
  Ret = FixedRet ++ VarRet,
  {RetTypes, _RetNames} = lists:unzip(Ret),
  Type = hipe_llvm:mk_struct(RetTypes),
  {RetStruct, I3} = mk_return_struct(Ret, Type),
  I4 = hipe_llvm:mk_ret([{Type, RetStruct}]),
  {[I4, I3, I2, I1], Relocs}.

%% @doc Create a structure to hold the return value and the precoloured
%%      registers.
mk_return_struct(RetValues, Type) ->
  mk_return_struct(RetValues, Type, [], "undef", 0).

mk_return_struct([], _, Acc, StructName, _) ->
  {StructName, Acc};
mk_return_struct([{ElemType, ElemName}|Rest], Type, Acc, StructName, Index) ->
  T1 = mk_temp(),
  I1 = hipe_llvm:mk_insertvalue(T1, Type, StructName, ElemType, ElemName,
                                integer_to_list(Index), []),
  mk_return_struct(Rest, Type, [I1 | Acc], T1, Index+1).

%%
%% store
%%
trans_store(I, Relocs) ->
  {Base, I1} = trans_src(hipe_rtl:store_base(I)),
  {Offset, I2} = trans_src(hipe_rtl:store_offset(I)),
  {Value, I3} = trans_src(hipe_rtl:store_src(I)),
  T1 = mk_temp(),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  I4 = hipe_llvm:mk_operation(T1, add, WordTy, Base, Offset, []),
  I5 =
    case hipe_rtl:store_size(I) of
      word ->
	T2 = mk_temp(),
	II1 = hipe_llvm:mk_conversion(T2, inttoptr, WordTy, T1, WordTyPtr),
	II2 = hipe_llvm:mk_store(WordTy, Value, WordTyPtr, T2, [], [],
				 false),
	[II2, II1];
      Size ->
	%% XXX: Is always trunc correct ?
	LoadType = llvm_type_from_size(Size),
	LoadTypePointer = hipe_llvm:mk_pointer(LoadType),
	T2 = mk_temp(),
	II1 = hipe_llvm:mk_conversion(T2, inttoptr, WordTy, T1, LoadTypePointer),
	T3 = mk_temp(),
	II2 = hipe_llvm:mk_conversion(T3, 'trunc', WordTy, Value, LoadType),
	II3 = hipe_llvm:mk_store(LoadType, T3, LoadTypePointer, T2, [], [], false),
	[II3, II2, II1]
    end,
  {[I5, I4, I3, I2, I1], Relocs}.

%%
%% switch
%%
trans_switch(I, Relocs, Data) ->
  RtlSrc = hipe_rtl:switch_src(I),
  {Src, I1} = trans_src(RtlSrc),
  Labels = hipe_rtl:switch_labels(I),
  JumpLabels = [mk_jump_label(L) || L <- Labels],
  SortOrder = hipe_rtl:switch_sort_order(I),
  NrLabels = length(Labels),
  ByteTyPtr = hipe_llvm:mk_pointer(hipe_llvm:mk_int(8)),
  TableType = hipe_llvm:mk_array(NrLabels, ByteTyPtr),
  TableTypeP = hipe_llvm:mk_pointer(TableType),
  TypedJumpLabels = [{hipe_llvm:mk_label_type(), X} || X <- JumpLabels],
  T1 = mk_temp(),
  {Src2, []} = trans_dst(RtlSrc),
  TableName = "table_" ++ tl(Src2),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  I2 = hipe_llvm:mk_getelementptr(T1, TableTypeP, "@"++TableName,
                                  [{WordTy, "0"}, {WordTy, Src}], false),
  T2 = mk_temp(),
  BYTE_TYPE_PP = hipe_llvm:mk_pointer(ByteTyPtr),
  I3 = hipe_llvm:mk_load(T2, BYTE_TYPE_PP, T1, [], [], false),
  I4 = hipe_llvm:mk_indirectbr(ByteTyPtr, T2, TypedJumpLabels),
  LMap = [{label, L} || L <- Labels],
  %% Update data with the info for the jump table
  {NewData, JTabLab} =
    case hipe_rtl:switch_sort_order(I) of
      [] ->
        hipe_consttab:insert_block(Data, word, LMap);
      SortOrder ->
        hipe_consttab:insert_sorted_block(Data, word, LMap, SortOrder)
    end,
  Relocs2 = relocs_store(TableName, {switch, {TableType, Labels, NrLabels,
					               SortOrder}, JTabLab}, Relocs),
  {[I4, I3, I2, I1], Relocs2, NewData}.

%% @doc Pass on RTL code in order to fix invoke and closure calls.
fix_code(Code) ->
  fix_calls(Code).

%% @doc Fix invoke calls and closure calls with more than ?NR_ARG_REGS
%%      arguments.
fix_calls(Code) ->
  fix_calls(Code, [], []).

fix_calls([], Acc, FailLabels) ->
  {lists:reverse(Acc), FailLabels};
fix_calls([I | Is], Acc, FailLabels) ->
  case hipe_rtl:is_call(I) of
    true ->
      {NewCall, NewFailLabels} =
        case hipe_rtl:call_fail(I) of
          [] ->
            {I, FailLabels};
          FailLabel ->
            fix_invoke_call(I, FailLabel, FailLabels)
        end,
      fix_calls(Is, [NewCall|Acc], NewFailLabels);
    false ->
      fix_calls(Is, [I|Acc], FailLabels)
  end.

%% @doc When a call has a fail continuation label it must be extended with a
%%      normal continuation label to go with the LLVM's invoke instruction.
%%      FailLabels is the list of labels of all fail blocks, which are needed to
%%      be declared as landing pads. Furtermore, we must add to fail labels a
%%      call to hipe_bifs:llvm_fix_pinned_regs/0 in order to avoid reloading old
%%      values of pinned registers. This may happen because the result of an
%%      invoke instruction is not available at fail-labels, and, thus, we cannot
%%      get the correct values of pinned registers. Finally, the stack needs to
%%      be re-adjusted when there are stack arguments.
fix_invoke_call(I, FailLabel, FailLabels) ->
  NewLabel = hipe_gensym:new_label(llvm),
  NewCall1 = hipe_rtl:call_normal_update(I, NewLabel),
  SpAdj = find_sp_adj(hipe_rtl:call_arglist(I)),
  case lists:keyfind(FailLabel, 1, FailLabels) of
    %% Same fail label with same Stack Pointer adjustment
    {FailLabel, NewFailLabel, SpAdj} ->
      NewCall2 = hipe_rtl:call_fail_update(NewCall1, NewFailLabel),
      {NewCall2, FailLabels};
    %% Same fail label but with different Stack Pointer adjustment
    {_, _, _} ->
      NewFailLabel = hipe_gensym:new_label(llvm),
      NewCall2 = hipe_rtl:call_fail_update(NewCall1, NewFailLabel),
      {NewCall2, [{FailLabel, NewFailLabel, SpAdj} | FailLabels]};
    %% New Fail label
    false ->
      NewFailLabel = hipe_gensym:new_label(llvm),
      NewCall2 = hipe_rtl:call_fail_update(NewCall1, NewFailLabel),
      {NewCall2, [{FailLabel, NewFailLabel, SpAdj} | FailLabels]}
  end.

find_sp_adj(ArgList) ->
  NrArgs = length(ArgList),
  case NrArgs > ?NR_ARG_REGS of
    true ->
      (NrArgs - ?NR_ARG_REGS) * hipe_rtl_arch:word_size();
    false ->
      0
  end.

%% @doc Add landingpad instruction in Fail Blocks.
add_landingpads(LLVM_Code, FailLabels) ->
  FailLabels2 = [convert_label(T) || T <- FailLabels],
  add_landingpads(LLVM_Code, FailLabels2, []).

add_landingpads([], _, Acc) ->
  lists:reverse(Acc);
add_landingpads([I | Is], FailLabels, Acc) ->
  case hipe_llvm:is_label(I) of
    true ->
      Label = hipe_llvm:label_label(I),
      Ins = create_fail_blocks(Label, FailLabels),
      add_landingpads(Is, FailLabels, [I | Ins] ++ Acc);
    false ->
      add_landingpads(Is, FailLabels, [I | Acc])
  end.

convert_label({X,Y,Z}) ->
  {"L" ++ integer_to_list(X), "FL" ++ integer_to_list(Y), Z}.

%% @doc Create a fail block wich.
create_fail_blocks(_, []) -> [];
create_fail_blocks(Label, FailLabels) ->
  create_fail_blocks(Label, FailLabels, []).

create_fail_blocks(Label, FailLabels, Acc) ->
  case lists:keytake(Label, 1, FailLabels) of
    false ->
      Acc;
    {value, {Label, FailLabel, SpAdj}, RestFailLabels} ->
      WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
      I1 = hipe_llvm:mk_label(FailLabel),
      LP = hipe_llvm:mk_landingpad(),
      I2 =
        case SpAdj > 0 of
          true ->
            StackPointer = ?ARCH_REGISTERS:reg_name(?ARCH_REGISTERS:sp()),
            hipe_llvm:mk_adj_stack(integer_to_list(SpAdj), StackPointer,
                                   WordTy);
          false -> []
        end,
      T1 = mk_temp(),
      FixedRegs = fixed_registers(),
      FunRetTy =
        hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
      I3 = hipe_llvm:mk_call(T1, false, "cc 11", [], FunRetTy,
			                       "@hipe_bifs.llvm_fix_pinned_regs.0", [], []),
      I4 = store_fixed_regs(FixedRegs, T1),
      I5 = hipe_llvm:mk_br("%" ++ Label),
      Ins = lists:flatten([I5, I4, I3, I2, LP,I1]),
      create_fail_blocks(Label, RestFailLabels, Ins ++ Acc)
  end.

%%------------------------------------------------------------------------------
%% Miscellaneous Functions
%%------------------------------------------------------------------------------

%% @doc Convert RTL argument list to LLVM argument list.
trans_args(ArgList) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  MakeArg =
    fun(A) ->
      {Name, I1} = trans_src(A),
      {{WordTy, Name}, I1}
    end,
  [MakeArg(A) || A <- ArgList].

%% @doc Convert a list of Precoloured registers to LLVM argument list.
fix_reg_args(ArgList) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  [{WordTy, A} || A <- ArgList].

%% @doc Load Precoloured registers.
load_fixed_regs(RegList) ->
  Names = [mk_temp_reg(R) || R <- RegList],
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  Fun1 =
    fun (X, Y) ->
      hipe_llvm:mk_load(X, WordTyPtr, "%" ++ Y ++ "_reg_var", [], [], false)
    end,
  Ins = lists:zipwith(Fun1, Names, RegList),
  {Names, Ins}.

%% @doc  Store Precoloured registers.
store_fixed_regs(RegList, Name) ->
  Names = [mk_temp_reg(R) || R <- RegList],
  Indexes = lists:seq(0, erlang:length(RegList) - 1),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  FunRetTy = hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
  Fun1 =
    fun(X,Y) ->
      hipe_llvm:mk_extractvalue(X, FunRetTy, Name, integer_to_list(Y), [])
    end,
  I1 = lists:zipwith(Fun1, Names, Indexes),
  Fun2 =
    fun (X, Y) ->
      hipe_llvm:mk_store(WordTy, X, WordTyPtr, "%" ++ Y ++ "_reg_var", [], [],
                         false)
    end,
  I2 = lists:zipwith(Fun2, Names, RegList),
  [I2, I1].

%%------------------------------------------------------------------------------
%% Translation of Names
%%------------------------------------------------------------------------------

%% @doc Fix F in MFA tuple to acceptable LLVM identifier (case of closure).
-spec fix_mfa_name(mfa()) -> mfa().
fix_mfa_name({Mod_Name, Closure_Name, Arity}) ->
  Fun_Name = list_to_atom(fix_closure_name(Closure_Name)),
  {Mod_Name, Fun_Name, Arity}.

%% @doc Make an acceptable LLVM identifier for a closure name.
fix_closure_name(ClosureName) ->
  make_llvm_id(atom_to_list(ClosureName)).

%% @doc Create an acceptable LLVM identifier.
make_llvm_id(Name) ->
  case Name of
    "" -> "Empty";
    Other -> lists:flatten([llvm_id(C) || C <- Other])
  end.

llvm_id(C) when C=:=46; C>47 andalso C<58; C>64 andalso C<91; C=:=95;
                C>96 andalso C<123 ->
  C;
llvm_id(C) ->
 io_lib:format("_~2.16.0B_",[C]).

%% @doc Create an acceptable LLVM identifier for an MFA.
trans_mfa_name({M,F,A}) ->
  N = atom_to_list(M) ++ "." ++ atom_to_list(F) ++ "." ++ integer_to_list(A),
  make_llvm_id(N).

%%------------------------------------------------------------------------------
%% Creation of Labels and Temporaries
%%------------------------------------------------------------------------------
mk_label(N) ->
  "L" ++ integer_to_list(N).

mk_jump_label(N) ->
  "%L" ++ integer_to_list(N).

mk_temp() ->
  "%t" ++ integer_to_list(hipe_gensym:new_var(llvm)).

mk_temp_reg(Name) ->
  "%" ++ Name ++ integer_to_list(hipe_gensym:new_var(llvm)).

%%----------------------------------------------------------------------------
%% Translation of Operands
%%----------------------------------------------------------------------------

store_stack_dst(TempDst, Dst) ->
  {Dst2, II1} = trans_dst(Dst),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  II2 = hipe_llvm:mk_store(WordTy, TempDst, WordTyPtr, Dst2, [], [], false),
  [II2, II1].

store_float_stack(TempDst, Dst) ->
  {Dst2, II1} = trans_dst(Dst),
  FloatTy = hipe_llvm:mk_double(),
  FloatTyPtr = hipe_llvm:mk_pointer(FloatTy),
  II2 = hipe_llvm:mk_store(FloatTy, TempDst, FloatTyPtr, Dst2, [], [], false),
  [II2, II1].

trans_float_src(Src) ->
  case hipe_rtl:is_const_label(Src) of
    true ->
      Name = "@DL" ++ integer_to_list(hipe_rtl:const_label_label(Src)),
      T1 = mk_temp(),
      %% XXX: Hardcoded offset
      ByteTy = hipe_llvm:mk_int(8),
      ByteTyPtr = hipe_llvm:mk_pointer(ByteTy),
      I1 = hipe_llvm:mk_getelementptr(T1, ByteTyPtr, Name,
           [{ByteTy, integer_to_list(?FLOAT_OFFSET)}], true),
      T2 = mk_temp(),
      FloatTy = hipe_llvm:mk_double(),
      FloatTyPtr = hipe_llvm:mk_pointer(FloatTy),
      I2 = hipe_llvm:mk_conversion(T2, bitcast, ByteTyPtr, T1, FloatTyPtr),
      T3 = mk_temp(),
      I3 = hipe_llvm:mk_load(T3, FloatTyPtr, T2, [], [], false),
      {T3, [I3, I2, I1]};
    false ->
      trans_src(Src)
  end.

trans_src(A) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  case hipe_rtl:is_imm(A) of
    true ->
      Value = integer_to_list(hipe_rtl:imm_value(A)),
      {Value, []};
    false ->
      case hipe_rtl:is_reg(A) of
        true ->
          case isPrecoloured(A) of
            true -> trans_reg(A, src);
            false ->
              {Name, []} = trans_reg(A, src),
              T1 = mk_temp(),
              I1 = hipe_llvm:mk_load(T1, WordTyPtr, Name, [], [], false),
              {T1, [I1]}
          end;
        false ->
          case hipe_rtl:is_var(A) of
            true ->
              RootName = "%vr" ++ integer_to_list(hipe_rtl:var_index(A)),
              T1 = mk_temp(),
              I1 = hipe_llvm:mk_load(T1, WordTyPtr, RootName, [], [], false),
              I2 =
		case hipe_rtl:var_liveness(A) of
		  live ->
		    [];
		  dead ->
		    NilValue = hipe_tagscheme:mk_nil(),
		    hipe_llvm:mk_store(WordTy, integer_to_list(NilValue), WordTyPtr, RootName,
		                       [], [], false)
		end,
              {T1, [I2, I1]};
            false ->
              case hipe_rtl:is_fpreg(A) of
                true ->
                  {Name, []} = trans_dst(A),
                  T1 = mk_temp(),
                  FloatTyPtr = hipe_llvm:mk_pointer(hipe_llvm:mk_double()),
                  I1 = hipe_llvm:mk_load(T1, FloatTyPtr, Name, [], [], false),
                  {T1, [I1]};
                false -> trans_dst(A)
              end
          end
      end
  end.

trans_dst(A) ->
  case hipe_rtl:is_reg(A) of
    true ->
      trans_reg(A, dst);
    false ->
      Name = case hipe_rtl:is_var(A) of
	       true ->
		 "%vr" ++ integer_to_list(hipe_rtl:var_index(A));
	       false ->
		 case hipe_rtl:is_fpreg(A) of
		   true -> "%fr" ++ integer_to_list(hipe_rtl:fpreg_index(A));
		   false ->
		     case hipe_rtl:is_const_label(A) of
		       true ->
			 "%DL" ++ integer_to_list(hipe_rtl:const_label_label(A)) ++ "_var";
		       false ->
			 exit({?MODULE, trans_dst, {"Bad RTL argument",A}})
		     end
		 end
	     end,
      {Name, []}
  end.

%% @doc Translate a register. If it is precoloured it must be mapped to the
%%      correct stack slot that holds the precoloured register value.
trans_reg(Arg, Position) ->
  Index = hipe_rtl:reg_index(Arg),
  case isPrecoloured(Arg) of
    true ->
      Name = map_precoloured_reg(Index),
      case Position of
        src -> fix_reg_src(Name);
        dst -> fix_reg_dst(Name)
      end;
    false ->
      {hipe_rtl_arch:reg_name(Index), []}
  end.

map_precoloured_reg(Index) ->
  case hipe_rtl_arch:reg_name(Index) of
    "%r15" -> "%hp_reg_var";
    "%rbp" -> "%p_reg_var";
    "%esi" -> "%hp_reg_var";
    "%ebp" -> "%p_reg_var";
    "%fcalls" ->
      {"%p_reg_var", ?ARCH_REGISTERS:proc_offset(?ARCH_REGISTERS:fcalls())};
    "%hplim" ->
      {"%p_reg_var", ?ARCH_REGISTERS:proc_offset(?ARCH_REGISTERS:heap_limit())};
    _ ->
      exit({?MODULE, map_precoloured_reg, {"Register not mapped yet", Index}})
  end.

%% @doc Load precoloured dst register.
fix_reg_dst(Register) ->
  case Register of
    {Name, Offset} -> %% Case of %fcalls, %hplim
      WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
      pointer_from_reg(Name, WordTy, Offset);
    Name -> %% Case of %p and %hp
      {Name, []}
  end.

%% @doc Load precoloured src register.
fix_reg_src(Register) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  case Register of
    {Name, Offset} -> %% Case of %fcalls, %hplim
      {T1, I1} = pointer_from_reg(Name, WordTy, Offset),
      T2 = mk_temp(),
      I2 = hipe_llvm:mk_load(T2, WordTyPtr, T1, [], [] , false),
      {T2, [I2, I1]};
    Name -> %% Case of %p and %hp
      T1 = mk_temp(),
      {T1, hipe_llvm:mk_load(T1, WordTyPtr, Name, [], [], false)}
  end.

%% @doc Load %fcalls and %hplim.
pointer_from_reg(RegName, Type, Offset) ->
  PointerType = hipe_llvm:mk_pointer(Type),
  T1 = mk_temp(),
  I1 = hipe_llvm:mk_load(T1, PointerType, RegName, [], [] ,false),
  T2 = mk_temp(),
  I2 = hipe_llvm:mk_conversion(T2, inttoptr, Type, T1, PointerType),
  T3 = mk_temp(),
  %% XXX: Offsets should be a power of 2.
  I3 = hipe_llvm:mk_getelementptr(T3, PointerType, T2,
    [{Type, integer_to_list(Offset div hipe_rtl_arch:word_size())}], true),
  {T3, [I3, I2, I1]}.

isPrecoloured(X) ->
  hipe_rtl_arch:is_precoloured(X).

%%------------------------------------------------------------------------------
%% Translation of operators
%%------------------------------------------------------------------------------

trans_op(Op) ->
  case Op of
    add -> add;
    sub -> sub;
    'or' -> 'or';
    'and' -> 'and';
    'xor' -> 'xor';
    sll -> shl;
    srl -> lshr;
    sra -> ashr;
    mul -> mul;
    'fdiv' -> fdiv;
    'sdiv' -> sdiv;
    'srem' -> srem;
    Other -> exit({?MODULE, trans_op, {"Unknown RTL operator", Other}})
  end.

trans_rel_op(Op) ->
  case Op of
    eq -> eq;
    ne -> ne;
    gtu -> ugt;
    geu -> uge;
    ltu -> ult;
    leu -> ule;
    gt -> sgt;
    ge -> sge;
    lt -> slt;
    le -> sle
  end.

trans_prim_op(Op) ->
  case Op of
    '+' -> "bif_add";
    '-' -> "bif_sub";
    '*' -> "bif_mul";
    'div' -> "bif_div";
    '/' -> "bif_div";
    Other -> atom_to_list(Other)
  end.

trans_fp_op(Op) ->
  case Op of
    fadd -> fadd;
    fsub -> fsub;
    fdiv -> fdiv;
    fmul -> fmul;
    fchs -> fsub;
    Other -> exit({?MODULE, trans_fp_op, {"Unknown RTL float operator",Other}})
  end.

%% Misc.
insn_dst(I) ->
  case I of
    #alu{} ->
      [hipe_rtl:alu_dst(I)];
    #alub{} ->
      [hipe_rtl:alub_dst(I)];
    #call{} ->
      case hipe_rtl:call_dstlist(I) of
        [] -> [];
        [Dst] -> [Dst]
      end;
    #load{} ->
      [hipe_rtl:load_dst(I)];
    #load_address{} ->
      [hipe_rtl:load_address_dst(I)];
    #load_atom{} ->
      [hipe_rtl:load_atom_dst(I)];
    #move{} ->
      [hipe_rtl:move_dst(I)];
    #phi{} ->
      [hipe_rtl:phi_dst(I)];
    #fconv{} ->
      [hipe_rtl:fconv_dst(I)];
    #fload{} ->
      [hipe_rtl:fload_dst(I)];
    #fmove{} ->
      [hipe_rtl:fmove_dst(I)];
    #fp{} ->
      [hipe_rtl:fp_dst(I)];
    #fp_unop{} ->
      [hipe_rtl:fp_unop_dst(I)];
    _ ->
      []
  end.

llvm_type_from_size(Size) ->
  case Size of
    byte  -> hipe_llvm:mk_int(8);
    int16 -> hipe_llvm:mk_int(16);
    int32 -> hipe_llvm:mk_int(32);
    word  -> hipe_llvm:mk_int(64)
  end.

%% @doc Create definition for the compiled function. The parameters that are
%%      passed to the stack must be reversed to match with the CC. Also
%%      precoloured registers that are passed as arguments must be stored to
%%      the corresonding stack slots.
create_function_definition(Fun, Params, Code, LocalVars) ->
  FunctionName = trans_mfa_name(Fun),
  FixedRegs = fixed_registers(),
  %% Reverse parameters to match with the Erlang calling convention
  ReversedParams =
    case erlang:length(Params) > ?NR_ARG_REGS of
		  false ->
		    Params;
		  true ->
		    {ParamsInRegs, ParamsInStack} = lists:split(?NR_ARG_REGS, Params),
		    ParamsInRegs ++ lists:reverse(ParamsInStack)
		end,
  Args = header_regs(FixedRegs) ++ header_params(ReversedParams),
  EntryLabel = hipe_llvm:mk_label("Entry"),
  FloatTy = hipe_llvm:mk_double(),
  ExceptionSync = hipe_llvm:mk_alloca("%exception_sync", FloatTy, [], []),
  I2 = load_regs(FixedRegs),
  I3 = hipe_llvm:mk_br(mk_jump_label(get(first_label))),
  StoredParams = store_params(Params),
  EntryBlock =
    lists:flatten([EntryLabel, ExceptionSync, I2, LocalVars, StoredParams, I3]),
  Final_Code = EntryBlock ++ Code,
  FunctionOptions = [nounwind, noredzone, list_to_atom("gc \"erlang\"")],
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  FunRetTy = hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
  hipe_llvm:mk_fun_def([], [], "cc 11", [], FunRetTy, FunctionName, Args,
                       FunctionOptions, [], Final_Code).

header_params(Params) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  [{WordTy, "%v" ++ integer_to_list(hipe_rtl:var_index(P))} || P <- Params].

store_params(Params) ->
  Fun1 =
    fun(X) ->
      Index = hipe_rtl:var_index(X),
      {Name, _} = trans_dst(X),
      ParamName = "%v" ++ integer_to_list(Index),
      WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
      WordTyPtr = hipe_llvm:mk_pointer(WordTy),
      hipe_llvm:mk_store(WordTy, ParamName, WordTyPtr, Name, [], [], false)
    end,
  lists:map(Fun1, Params).

fixed_registers() ->
  case get(hipe_target_arch) of
    x86 ->
      ["hp", "p"];
    amd64 ->
      ["hp", "p"];
    Other ->
      exit({?MODULE, map_registers, {"Unknown architecture", Other}})
  end.

header_regs(Registers) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  [{WordTy, "%" ++ X ++ "_in"} || X <- Registers].

load_regs(Registers) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  Fun1 =
    fun(X) ->
	I1 = hipe_llvm:mk_alloca("%" ++ X ++ "_reg_var", WordTy, [], []),
	I2 = hipe_llvm:mk_store(WordTy, "%" ++ X ++ "_in", WordTyPtr,
			 "%" ++ X ++ "_reg_var", [], [], false),
	[I1, I2]
    end,
  lists:map(Fun1, Registers).

%%------------------------------------------------------------------------------
%% Relocation-specific Stuff
%%------------------------------------------------------------------------------

relocs_store(Key, Value, Relocs) ->
  dict:store(Key, Value, Relocs).

relocs_to_list(Relocs) ->
  dict:to_list(Relocs).

%% @doc This function is responsible for the actions needed to handle
%%      relocations:
%%      1) Updates relocations with constants and switch jump tables.
%%      2) Creates LLVM code to declare relocations as external
%%         functions/constants.
%%      3) Creates LLVM code in order to create local variables for the external
%%         constants/labels.
handle_relocations(Relocs, Data, Fun) ->
  RelocsList = relocs_to_list(Relocs),
  %% Seperate Relocations according to their type
  {CallList, AtomList, ClosureList, ClosureLabels, SwitchList} =
    seperate_relocs(RelocsList),
  %% Create code to declare atoms
  AtomDecl = [declare_atom(A) || A <- AtomList],
  %% Create code to create local name for atoms
  AtomLoad = [load_atom(A) || A <- AtomList],
  %% Create code to declare closures
  ClosureDecl = [declare_closure(C) || C <- ClosureList],
  %% Create code to create local name for closures
  ClosureLoad = [load_closure(C) || C <- ClosureList],
  %% Find function calls
  IsExternalCall = fun (X) -> is_external_call(X, Fun) end,
  ExternalCallList = lists:filter(IsExternalCall, CallList),
  %% Create code to declare external function
  FunDecl = fixed_fun_decl() ++ [call_to_decl(C) || C <- ExternalCallList],
  %% Extract constant labels from Constant Map (remove duplicates)
  ConstLabels = hipe_consttab:labels(Data),
  %% Create code to declare constants
  ConstDecl = [declare_constant(C) || C <- ConstLabels],
  %% Create code to create local name for constants
  ConstLoad = [load_constant(C) || C <- ConstLabels],
  %% Create code to create jump tables
  SwitchDecl = declare_switches(SwitchList, Fun),
  %% Create code to create a table with the labels of all closure calls
  {ClosureLabelDecl, Relocs1} =
    declare_closure_labels(ClosureLabels, Relocs, Fun),
  %% Enter constants to relocations
  Relocs2 = lists:foldl(fun const_to_dict/2, Relocs1, ConstLabels),
  %% Temporary Store inc_stack and llvm_fix_pinned_regs to Dictionary
  %% TODO: Remove this
  Relocs3 = dict:store("inc_stack_0", {call, {bif, inc_stack_0, 0}}, Relocs2),
  Relocs4 = dict:store("hipe_bifs.llvm_fix_pinned_regs.0",
                       {call, {hipe_bifs, llvm_fix_pinned_regs, 0}}, Relocs3),
  BranchMetaData = [
    hipe_llvm:mk_meta(?BRANCH_META_TAKEN,     ["branch_weights", 99, 1])
  , hipe_llvm:mk_meta(?BRANCH_META_NOT_TAKEN, ["branch_weights", 1, 99])
  ],
  ExternalDeclarations = AtomDecl ++ ClosureDecl ++ ConstDecl ++ FunDecl ++
    ClosureLabelDecl ++ SwitchDecl ++ BranchMetaData,
  LocalVariables = AtomLoad ++ ClosureLoad ++ ConstLoad,
  {Relocs4, ExternalDeclarations, LocalVariables}.

%% @doc Seperate relocations according to their type.
seperate_relocs(Relocs) ->
  seperate_relocs(Relocs, [], [], [], [], []).

seperate_relocs([], CallAcc, AtomAcc, ClosureAcc, LabelAcc, JmpTableAcc) ->
  {CallAcc, AtomAcc, ClosureAcc, LabelAcc, JmpTableAcc};
seperate_relocs([R|Rs], CallAcc, AtomAcc, ClosureAcc, LabelAcc, JmpTableAcc) ->
  case R of
    {_, {call, _}} ->
      seperate_relocs(Rs, [R | CallAcc], AtomAcc, ClosureAcc, LabelAcc,
                      JmpTableAcc);
    {_, {atom, _}} ->
      seperate_relocs(Rs, CallAcc, [R | AtomAcc], ClosureAcc, LabelAcc,
                      JmpTableAcc);
    {_, {closure, _}} ->
      seperate_relocs(Rs, CallAcc, AtomAcc, [R | ClosureAcc], LabelAcc,
                      JmpTableAcc);
    {_, {switch, _, _}} ->
      seperate_relocs(Rs, CallAcc, AtomAcc, ClosureAcc, LabelAcc,
                      [R | JmpTableAcc]);
    {_, {closure_label, _, _}} ->
      seperate_relocs(Rs, CallAcc, AtomAcc, ClosureAcc, [R | LabelAcc],
                      JmpTableAcc)
  end.

%% @doc External declaration of an atom.
declare_atom({AtomName, _}) ->
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  hipe_llvm:mk_const_decl("@" ++ AtomName, "external constant", WordTy, "").

%% @doc Creation of local variable for an atom.
load_atom({AtomName, _}) ->
  Dst = "%" ++ AtomName ++ "_var",
  Name = "@" ++ AtomName,
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  WordTyPtr = hipe_llvm:mk_pointer(WordTy),
  hipe_llvm:mk_conversion(Dst, ptrtoint, WordTyPtr, Name, WordTy).

%% @doc External declaration of a closure.
declare_closure({ClosureName, _})->
  ByteTy = hipe_llvm:mk_int(8),
  hipe_llvm:mk_const_decl("@" ++ ClosureName, "external constant", ByteTy, "").

%% @doc Creation of local variable for a closure.
load_closure({ClosureName, _})->
  Dst = "%" ++ ClosureName ++ "_var",
  Name = "@" ++ ClosureName,
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  ByteTyPtr = hipe_llvm:mk_pointer(hipe_llvm:mk_int(8)),
  hipe_llvm:mk_conversion(Dst, ptrtoint, ByteTyPtr, Name, WordTy).

%% @doc Declaration of a local variable for a switch jump table.
declare_switches(JumpTableList, Fun) ->
  FunName = trans_mfa_name(Fun),
  [declare_switch_table(X, FunName) || X <- JumpTableList].

declare_switch_table({Name, {switch, {TableType, Labels, _, _}, _}}, FunName) ->
  LabelList = [mk_jump_label(L) || L <- Labels],
  Fun1 = fun(X) -> "i8* blockaddress(@" ++ FunName ++ ", " ++ X ++ ")" end,
  List2 = lists:map(Fun1, LabelList),
  List3 = string:join(List2, ",\n"),
  List4 = "[\n" ++ List3 ++ "\n]\n",
  hipe_llvm:mk_const_decl("@" ++ Name, "constant", TableType, List4).

%% @doc Declaration of a variable for a table with the labels of all closure
%%      calls in the code.
declare_closure_labels([], Relocs, _Fun) ->
  {[], Relocs};
declare_closure_labels(ClosureLabels, Relocs, Fun) ->
  FunName = trans_mfa_name(Fun),
  {LabelList, ArityList} =
    lists:unzip([{mk_jump_label(Label), A} ||
		  {_, {closure_label, Label, A}} <- ClosureLabels]),
  Relocs1 = relocs_store("table_closures", {table_closures, ArityList}, Relocs),
  List2 =
    ["i8* blockaddress(@" ++ FunName ++ ", " ++ L ++ ")" || L <- LabelList],
  List3 = string:join(List2, ",\n"),
  List4 = "[\n" ++ List3 ++ "\n]\n",
  NrLabels = length(LabelList),
  ByteTyPtr = hipe_llvm:mk_pointer(hipe_llvm:mk_int(8)),
  TableType = hipe_llvm:mk_array(NrLabels, ByteTyPtr),
  ConstDecl =
    hipe_llvm:mk_const_decl("@table_closures", "constant", TableType, List4),
  {[ConstDecl], Relocs1}.

%% @doc A call is treated as non external only in a case of a recursive
%%      function.
is_external_call({_, {call, Fun}}, Fun) -> false;
is_external_call(_, _) -> true.

%% @doc External declaration of a function.
call_to_decl({Name, {call, MFA}}) ->
  {M, _F, A} = MFA,
  CConv = "cc 11",
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  FunRetTy = hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
  {Type, Args} =
    case M of
      llvm ->
        {hipe_llvm:mk_struct([WordTy, hipe_llvm:mk_int(1)]), [1, 2]};
      %% +precoloured regs
      _ ->
        {FunRetTy, lists:seq(1, A + ?NR_PINNED_REGS)}
    end,
  ArgsTypes = lists:duplicate(length(Args), WordTy),
  hipe_llvm:mk_fun_decl([], [], CConv, [], Type, "@" ++ Name, ArgsTypes, []).

%% @doc These functions are always declared, even if not used.
fixed_fun_decl() ->
  ByteTy = hipe_llvm:mk_int(8),
  ByteTyPtr = hipe_llvm:mk_pointer(ByteTy),
  LandPad = hipe_llvm:mk_fun_decl([], [], [], [], hipe_llvm:mk_int(32),
    "@__gcc_personality_v0", [hipe_llvm:mk_int(32), hipe_llvm:mk_int(64),
    ByteTyPtr, ByteTyPtr], []),
  GCROOTDecl = hipe_llvm:mk_fun_decl([], [], [], [], hipe_llvm:mk_void(),
    "@llvm.gcroot", [hipe_llvm:mk_pointer(ByteTyPtr), ByteTyPtr], []),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  FunRetTy = hipe_llvm:mk_struct(lists:duplicate(?NR_PINNED_REGS + 1, WordTy)),
  FixPinnedRegs = hipe_llvm:mk_fun_decl([], [], [], [], FunRetTy,
    "@hipe_bifs.llvm_fix_pinned_regs.0", [], []),
  GcMetadata = hipe_llvm:mk_const_decl("@gc_metadata", "external constant",
                                       ByteTy, ""),
  [LandPad, GCROOTDecl, FixPinnedRegs, GcMetadata].

%% @doc Declare an External Consant. We declare all constants as i8 in order to
%%      be able to calcucate pointers of the form DL+6, with the getelementptr
%%      instruction. Otherwise we have to convert constants form pointers to
%%      values, add the offset and convert them again to pointers.
declare_constant(Label) ->
  Name = "@DL" ++ integer_to_list(Label),
  ByteTy = hipe_llvm:mk_int(8),
  hipe_llvm:mk_const_decl(Name, "external constant", ByteTy, "").

%% @doc Load a constant is achieved by converting a pointer to an integer of
%%      the correct width.
load_constant(Label) ->
  Dst = "%DL" ++ integer_to_list(Label) ++ "_var",
  Name = "@DL" ++ integer_to_list(Label),
  WordTy = hipe_llvm:mk_int(?WORD_WIDTH),
  ByteTyPtr = hipe_llvm:mk_pointer(hipe_llvm:mk_int(8)),
  hipe_llvm:mk_conversion(Dst, ptrtoint, ByteTyPtr, Name, WordTy).

%% @doc Store external constants and calls to dictionary.
const_to_dict(Elem, Dict) ->
  Name = "DL" ++ integer_to_list(Elem),
  dict:store(Name, {'constant', Elem}, Dict).

%% @doc Export the hipe literals that LLVM needs to generate the prologue as
%% metadata.
add_literals_metadata(ExternalDecls) ->
  Pairs = [hipe_llvm:mk_meta(integer_to_list(?FIRST_FREE_META_NO),
			     ["P_NSP_LIMIT", ?P_NSP_LIMIT])
	  ,hipe_llvm:mk_meta(integer_to_list(?FIRST_FREE_META_NO + 1),
			     ["X86_LEAF_WORDS", ?X86_LEAF_WORDS])
	  ,hipe_llvm:mk_meta(integer_to_list(?FIRST_FREE_META_NO + 2),
			     ["AMD64_LEAF_WORDS", ?AMD64_LEAF_WORDS])
	  ],
  [hipe_llvm:mk_meta(?HIPE_LITERALS_META, Pairs) |
   Pairs ++ ExternalDecls].
