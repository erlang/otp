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
%%--------------------------------------------------------------------
%% File    : hipe_icode_fp.erl
%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%% Description : One pass analysis to find floating point values. 
%%               Mapping to FP variables and creation of FP EBBs.
%%
%% Created : 23 Apr 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%--------------------------------------------------------------------

-module(hipe_icode_fp).

-export([cfg/1]).

-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").

-type mapped_fvar()     :: icode_fvar() | {assigned, icode_fvar()} .
-type incoming_fvars()  :: [{icode_lbl(), mapped_fvar()}].
-type initial_var_map() :: #{icode_var() => incoming_fvars()}.
-type bb_phi_list()     :: [{icode_fvar(), [{icode_lbl(), icode_fvar()}]}].
-type var_map_phi()     :: #{phi => bb_phi_list(),
			     icode_var() => mapped_fvar()}.
-type var_map()         :: #{icode_var() => mapped_fvar()}.

-type edge()            :: {icode_lbl(), icode_lbl()}.
-type edge_map()        :: #{edge() => var_map()}.

-type worklist(Item)    :: {[Item], [Item], gb_sets:set(Item)}.
-type worklist()        :: worklist(icode_lbl()).

-type fail_lbl()        :: [] | icode_lbl().
-type in_block()        :: {true, fail_lbl()} | false.
-type fp_ebb_map()      :: #{{inblock_in | inblock_out, icode_lbl()} | edge()
			     => in_block()}.

-record(state, {edge_map   = #{} :: edge_map(),
		fp_ebb_map = #{} :: fp_ebb_map(),
		cfg              :: cfg()}).
-type state() :: #state{}.

-type icode_phi()      :: #icode_phi{}.
-type icode_variable() :: #icode_variable{}.
-type icode_const()    :: #icode_const{}.

%%--------------------------------------------------------------------

-spec cfg(cfg()) -> cfg().

cfg(Cfg) ->
  %%hipe_icode_cfg:pp(Cfg),
  NewCfg = annotate_fclearerror(Cfg),
  State = new_state(NewCfg),
  NewState = place_fp_blocks(State),
  %% hipe_icode_cfg:pp(state__cfg(NewState)),
  NewState2 = finalize(NewState),
  NewCfg1 = state__cfg(NewState2),
  %% hipe_icode_cfg:pp(NewCfg1),
  NewCfg2 = unannotate_fclearerror(NewCfg1),
  NewCfg2.

%%--------------------------------------------------------------------
%% Annotate fclearerror with information of the fail label of the
%% corresponding fcheckerror.
%%--------------------------------------------------------------------

-spec annotate_fclearerror(cfg()) -> cfg().

annotate_fclearerror(Cfg) ->
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  annotate_fclearerror(Labels, Cfg).

-spec annotate_fclearerror([icode_lbl()], cfg()) -> cfg().

annotate_fclearerror([Label|Left], Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = annotate_fclearerror1(Code, Label, Cfg, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  annotate_fclearerror(Left, NewCfg);
annotate_fclearerror([], Cfg) ->
  Cfg.

-spec annotate_fclearerror1(icode_instrs(), icode_lbl(), cfg(), icode_instrs())
			   -> icode_instrs().

annotate_fclearerror1([I|Left], Label, Cfg, Acc) ->
  case I of
    #icode_call{} ->
      case hipe_icode:call_fun(I) of
	fclearerror ->
	  Fail = lookahead_for_fcheckerror(Left, Label, Cfg),
	  NewI = hipe_icode:call_fun_update(I, {fclearerror, Fail}),
	  annotate_fclearerror1(Left, Label, Cfg, [NewI|Acc]);
	_ ->
	  annotate_fclearerror1(Left, Label, Cfg, [I|Acc])
      end;
    _ ->
      annotate_fclearerror1(Left, Label, Cfg, [I|Acc])
  end;
annotate_fclearerror1([], _Label, _Cfg, Acc) ->
  lists:reverse(Acc).

-spec lookahead_for_fcheckerror(icode_instrs(), icode_lbl(), cfg()) ->
				   fail_lbl().

lookahead_for_fcheckerror([I|Left], Label, Cfg) ->
  case I of
    #icode_call{} ->
      case hipe_icode:call_fun(I) of
	fcheckerror ->
	  hipe_icode:call_fail_label(I);
	_ ->
	  lookahead_for_fcheckerror(Left, Label, Cfg)
      end;
    _ ->
       lookahead_for_fcheckerror(Left, Label, Cfg)
  end;
lookahead_for_fcheckerror([], Label, Cfg) ->
  case hipe_icode_cfg:succ(Cfg, Label) of
    [] -> exit("Unterminated fp ebb");
    SuccList ->
      Succ = hd(SuccList),
      Code = hipe_bb:code(hipe_icode_cfg:bb(Cfg, Label)),
      lookahead_for_fcheckerror(Code, Succ, Cfg)
  end.

-spec unannotate_fclearerror(cfg()) -> cfg().

unannotate_fclearerror(Cfg) ->
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  unannotate_fclearerror(Labels, Cfg).

-spec unannotate_fclearerror([icode_lbl()], cfg()) -> cfg().

unannotate_fclearerror([Label|Left], Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = unannotate_fclearerror1(Code, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  unannotate_fclearerror(Left, NewCfg);
unannotate_fclearerror([], Cfg) ->
  Cfg.

-spec unannotate_fclearerror1(icode_instrs(), icode_instrs()) ->
				 icode_instrs().

unannotate_fclearerror1([I|Left], Acc) ->
  case I of
    #icode_call{} ->
      case hipe_icode:call_fun(I) of
	{fclearerror, _Fail} ->
	  NewI = hipe_icode:call_fun_update(I, fclearerror),
	  unannotate_fclearerror1(Left, [NewI|Acc]);
	_ ->
	  unannotate_fclearerror1(Left, [I|Acc])
      end;
    _ ->
      unannotate_fclearerror1(Left, [I|Acc])
  end;
unannotate_fclearerror1([], Acc) ->
  lists:reverse(Acc).

%%--------------------------------------------------------------------
%% Make float EBBs
%%--------------------------------------------------------------------

-spec place_fp_blocks(state()) -> state().

place_fp_blocks(State) ->
  WorkList = new_worklist(State),
  transform_block(WorkList, State).

-spec transform_block(worklist(), state()) -> state().

transform_block(WorkList, State) ->
  case get_work(WorkList) of
    none ->
      State;
    {Label, NewWorkList} ->      
      %%io:format("Handling ~w \n", [Label]),
      BB = state__bb(State, Label),
      Code1 = hipe_bb:butlast(BB),
      Last = hipe_bb:last(BB),
      NofPreds = length(state__pred(State, Label)),
      Map = state__map(State, Label),
      FilteredMap = filter_map(Map, NofPreds),
      {Prelude, NewFilteredMap} = do_prelude(FilteredMap),

      %% Take care to have a map without any new bindings from the
      %% last instruction if it can fail.
      {FailMap, NewCode1} = transform_instrs(Code1, Map, NewFilteredMap, []),
      {NewMap, NewCode2} = transform_instrs([Last], Map, FailMap, []),
      SuccSet0 = ordsets:from_list(hipe_icode:successors(Last)),
      FailSet = ordsets:from_list(hipe_icode:fails_to(Last)),
      SuccSet = ordsets:subtract(SuccSet0, FailSet),
      NewCode = NewCode1 ++ NewCode2,
      NewBB = hipe_bb:code_update(BB, Prelude++NewCode),
      NewState = state__bb_add(State, Label, NewBB),
      case update_maps(NewState, Label, SuccSet, NewMap, FailSet, FailMap) of
	fixpoint ->
	  transform_block(NewWorkList, NewState);
	{NewState1, AddBlocks} ->
	  NewWorkList1 = add_work(NewWorkList, AddBlocks),
	  transform_block(NewWorkList1, NewState1)
      end
  end.

-spec update_maps(state(), icode_lbl(), ordsets:ordset(icode_lbl()),
		  var_map(), ordsets:ordset(icode_lbl()), var_map())
		 -> fixpoint | {state(), [icode_lbl()]}.

update_maps(State, Label, SuccSet, SuccMap, FailSet, FailMap) ->
  {NewState, Add1} = update_maps(State, Label, SuccSet, SuccMap, []),
  case update_maps(NewState, Label, FailSet, FailMap, Add1) of
    {_NewState1, []} -> fixpoint;
    {_NewState1, _Add} = Ret -> Ret
  end.

-spec update_maps(state(), icode_lbl(), ordsets:ordset(icode_lbl()),
		  var_map(), [icode_lbl()])
		 -> {state(), [icode_lbl()]}.

update_maps(State, From, [To|Left], Map, Acc) ->
  case state__map_update(State, From, To, Map) of
    fixpoint ->
      update_maps(State, From, Left, Map, Acc);
    NewState ->
      update_maps(NewState, From, Left, Map, [To|Acc])
  end;
update_maps(State, _From, [], _Map, Acc) ->
  {State, Acc}.

-spec transform_instrs(icode_instrs(), edge_map(), var_map(), icode_instrs())
		      -> {var_map(), icode_instrs()}.

transform_instrs([I|Left], PhiMap, Map, Acc) ->
  Defines = hipe_icode:defines(I),
  NewMap = maps:without(Defines, Map),
  NewPhiMap = maps:without(Defines, PhiMap),
  case I of
    #icode_phi{} ->
      Uses = hipe_icode:uses(I),
      case [X || X <- Uses, lookup(X, PhiMap) =/= none] of
	[] ->
	  %% No ordinary variables from the argument have been untagged.
	  transform_instrs(Left, NewPhiMap, NewMap, [I|Acc]);
	Uses ->
	  %% All arguments are untagged. Let's untag the destination.
	  Dst = hipe_icode:phi_dst(I),
	  NewDst = hipe_icode:mk_new_fvar(),
	  NewMap1 = NewMap#{Dst => NewDst},
	  NewI = subst_phi_uncond(I, NewDst, PhiMap),
	  transform_instrs(Left, NewPhiMap, NewMap1, [NewI|Acc]);
	_ ->
	  %% Some arguments are untagged. Keep the destination.
	  Dst = hipe_icode:phi_dst(I),
	  NewI = subst_phi(I, Dst, PhiMap),
	  transform_instrs(Left, NewPhiMap, NewMap, [NewI|Acc])
      end;
    #icode_call{} ->
      case hipe_icode:call_fun(I) of
	X when X =:= unsafe_untag_float orelse X =:= conv_to_float ->
	  [Dst] = hipe_icode:defines(I),
	  case hipe_icode:uses(I) of
	    [] -> %% Constant
	      transform_instrs(Left, NewPhiMap, NewMap, [I|Acc]);
	    [Src] ->
	      case lookup(Src, Map) of
		none ->
		  NewMap1 = NewMap#{Src => {assigned, Dst}},
		  transform_instrs(Left, NewPhiMap, NewMap1, [I|Acc]);
		Dst ->
		  %% This is the instruction that untagged the variable.
		  %% Use old maps.
		  transform_instrs(Left, NewPhiMap, Map, [I|Acc]);
		FVar ->
		  %% The variable was already untagged. 
		  %% This instruction can be changed to a move.
		  NewI = hipe_icode:mk_move(Dst, FVar),
		  case hipe_icode:call_continuation(I) of
		    [] ->
		      transform_instrs(Left,NewPhiMap,NewMap,[NewI|Acc]);
		    ContLbl ->
		      Goto = hipe_icode:mk_goto(ContLbl),
		      transform_instrs(Left, NewPhiMap, NewMap, 
				       [Goto, NewI|Acc])
		  end
	      end
	  end;
	unsafe_tag_float ->
	  [Dst] = hipe_icode:defines(I),
	  [Src] = hipe_icode:uses(I),
	  NewMap1 = NewMap#{Dst => {assigned, Src}},
	  transform_instrs(Left, NewPhiMap, NewMap1,[I|Acc]);
	_ ->
	  {NewMap1, NewAcc} = check_for_fop_candidates(I, NewMap, Acc),
	  transform_instrs(Left, NewPhiMap, NewMap1, NewAcc)
      end;
    _ ->
      NewIns = handle_untagged_arguments(I, NewMap),
      transform_instrs(Left, NewPhiMap, NewMap, NewIns ++ Acc)
  end;
transform_instrs([], _PhiMap, Map, Acc) ->
  {Map, lists:reverse(Acc)}.

-spec check_for_fop_candidates(icode_instr(), var_map(), icode_instrs())
			      -> {var_map(), icode_instrs()}.

check_for_fop_candidates(I, Map, Acc) ->
  case is_fop_cand(I) of
    false ->
      NewIs = handle_untagged_arguments(I, Map),
      {Map, NewIs ++ Acc};
    true ->
      Fail = hipe_icode:call_fail_label(I),
      Cont = hipe_icode:call_continuation(I),
      Op = fun_to_fop(hipe_icode:call_fun(I)), 
      case Fail of
	[] ->
	  Args = hipe_icode:args(I),
	  ConstArgs = [X || X <- Args, hipe_icode:is_const(X)],
	  try lists:foreach(fun(X) -> float(hipe_icode:const_value(X)) end, 
			    ConstArgs) of
	    ok ->
	      %%io:format("Changing ~w to ~w\n", [hipe_icode:call_fun(I), Op]),
	      Uses = hipe_icode:uses(I),
	      Defines = hipe_icode:defines(I),
	      Convs = [X||X <- remove_duplicates(Uses), lookup(X,Map) =:= none],
	      NewMap0 = add_new_bindings_assigned(Convs, Map),
	      NewMap = add_new_bindings_unassigned(Defines, NewMap0),
	      ConvIns = get_conv_instrs(Convs, NewMap),
	      NewI = hipe_icode:mk_primop(lookup_list(Defines, NewMap), Op,
					  lookup_list_keep_consts(Args,NewMap),
					  Cont, Fail),
	      NewI2 = conv_consts(ConstArgs, NewI),
	      {NewMap, [NewI2|ConvIns]++Acc}
	  catch 
	    error:badarg ->
	      %% This instruction will fail at runtime. The warning
	      %% should already have happened in hipe_icode_type.
	      NewIs = handle_untagged_arguments(I, Map),
	      {Map, NewIs ++ Acc}
	  end;
	_ -> %% Bailing out! Can't handle instructions in catches (yet).
	  NewIs = handle_untagged_arguments(I, Map),
	  {Map, NewIs ++ Acc}
      end
  end.


-spec handle_untagged_arguments(icode_instr(), var_map()) -> icode_instrs().

%% If this is an instruction that needs to operate on tagged values,
%% which currently are untagged, we must tag the values and perhaps
%% end the fp ebb.

handle_untagged_arguments(I, Map) ->
  case [X || X <- hipe_icode:uses(I), must_be_tagged(X, Map)] of
    [] ->
      [I];
    Tag ->
      TagIntrs = 
	[hipe_icode:mk_primop([Dst], unsafe_tag_float, 
			      [maps:get(Dst, Map)]) || Dst <- Tag],
      [I|TagIntrs]
  end.

-spec do_prelude(var_map_phi()) -> {[icode_phi()], var_map()}.

%% Add phi nodes for untagged fp values.

do_prelude(Map = #{phi := List}) ->
  %%io:format("Adding phi: ~w\n", [List]),
  Fun = fun ({FVar, Bindings}, Acc) ->
	    [hipe_icode:mk_phi(FVar, Bindings)|Acc]
	end,
  {lists:foldl(Fun, [], List), maps:remove(phi, Map)};
do_prelude(Map) -> {[], Map}.

-spec split_code([I]) -> {[I], I} when
    I :: icode_instr().

split_code(Code) ->
  split_code(Code, []).

split_code([I], Acc) ->
  {lists:reverse(Acc), I};
split_code([I|Left], Acc) ->
  split_code(Left, [I|Acc]).


-spec finalize(state()) -> state().

%% When all code is mapped to fp instructions we must make sure that
%% the fp ebb information going into each block is the same as the
%% information coming out of each predecessor. Otherwise, we must add
%% a block in between.

finalize(State) ->
  Worklist = new_worklist(State),
  NewState = place_error_handling(Worklist, State),
  Edges = needs_fcheckerror(NewState),
  finalize(Edges, NewState).

-spec finalize([edge()], state()) -> state().

finalize([{From, To}|Left], State) ->
  NewState = add_fp_ebb_fixup(From, To, State),
  finalize(Left, NewState);
finalize([], State) ->
  State.

-spec needs_fcheckerror(state()) -> [{none | icode_lbl(), icode_lbl()}].

needs_fcheckerror(State) ->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:labels(Cfg),
  needs_fcheckerror(Labels, State, []).

-spec needs_fcheckerror([icode_lbl()], state(),
			[{none | icode_lbl(), icode_lbl()}])
		       -> [{none | icode_lbl(), icode_lbl()}].

needs_fcheckerror([Label|Left], State, Acc) ->
  case state__get_in_block_in(State, Label) of
    {true, _} ->
      needs_fcheckerror(Left, State, Acc);
    false ->
      Pred = state__pred(State, Label),
      case [X || X <- Pred, state__get_in_block_out(State, X) =/= false] of
	[] ->
	  needs_fcheckerror(Left, State, Acc);
	NeedsFcheck ->	  
	  case length(Pred) =:= length(NeedsFcheck) of
	    true ->
	      %% All edges need fcheckerror. Add this to the
	      %% beginning of the block instead.
	      needs_fcheckerror(Left, State, [{none, Label}|Acc]);
	    false ->
	      Edges = [{X, Label} || X <- NeedsFcheck],
	      needs_fcheckerror(Left, State, Edges ++ Acc)
	  end
      end
  end;
needs_fcheckerror([], _State, Acc) ->
  Acc.

-spec add_fp_ebb_fixup(none | icode_lbl(), icode_lbl(), state()) -> state().

add_fp_ebb_fixup('none', To, State) ->
  %% Add the fcheckerror to the start of the block.
  BB = state__bb(State, To),
  Code = hipe_bb:code(BB),
  Phis = lists:takewhile(fun(X) -> hipe_icode:is_phi(X) end, Code),
  TailCode = lists:dropwhile(fun(X) -> hipe_icode:is_phi(X) end, Code),
  FC = hipe_icode:mk_primop([], fcheckerror, []),
  NewCode = Phis ++ [FC|TailCode],
  state__bb_add(State, To, hipe_bb:code_update(BB, NewCode));
add_fp_ebb_fixup(From, To, State) ->
  FCCode = [hipe_icode:mk_primop([], fcheckerror, [], To, [])],
  FCBB = hipe_bb:mk_bb(FCCode),
  FCLabel = hipe_icode:label_name(hipe_icode:mk_new_label()),
  NewState = state__bb_add(State, FCLabel, FCBB),
  NewState1 = state__redirect(NewState, From, To, FCLabel),
  ToBB = state__bb(NewState, To),
  ToCode = hipe_bb:code(ToBB),
  NewToCode = redirect_phis(ToCode, From, FCLabel),
  NewToBB = hipe_bb:code_update(ToBB, NewToCode),
  state__bb_add(NewState1, To, NewToBB).

-spec redirect_phis(icode_instrs(), icode_lbl(), icode_lbl())
		   -> icode_instrs().

redirect_phis(Code, OldFrom, NewFrom) ->
  redirect_phis(Code, OldFrom, NewFrom, []).

-spec redirect_phis(icode_instrs(), icode_lbl(), icode_lbl(), icode_instrs())
		   -> icode_instrs().

redirect_phis([I|Is] = Code, OldFrom, NewFrom, Acc) ->
  case I of
    #icode_phi{} ->
      NewI = hipe_icode:phi_redirect_pred(I, OldFrom, NewFrom),
      redirect_phis(Is, OldFrom, NewFrom, [NewI|Acc]);
    _ ->
      lists:reverse(Acc, Code)
  end;
redirect_phis([], _OldFrom, _NewFrom, Acc) ->
  lists:reverse(Acc).

-spec subst_phi(icode_phi(), icode_variable(), edge_map())
	       -> icode_phi().

subst_phi(I, Dst, Map) ->
  ArgList = subst_phi_uses0(hipe_icode:phi_arglist(I), Map, []),
  hipe_icode:mk_phi(Dst, ArgList).

-spec subst_phi_uses0([{icode_lbl(), icode_variable()}], edge_map(),
		      [{icode_lbl(), icode_variable()}])
		     -> [{icode_lbl(), icode_variable()}].

subst_phi_uses0([{Pred, Var}|Left], Map, Acc) ->
  case Map of
    #{Var := List} ->
      case lists:keyfind(Pred, 1, List) of
	{Pred, {assigned, _NewVar}} -> 
	  %% The variable is untagged, but it has been assigned. Keep it!
	  subst_phi_uses0(Left, Map, [{Pred, Var} | Acc]);
	{Pred, _NewVar} = PredNV ->
	  %% The variable is untagged and it has never been assigned as tagged.
	  subst_phi_uses0(Left, Map, [PredNV | Acc]);
	false ->
	  %% The variable is not untagged.
	  subst_phi_uses0(Left, Map, [{Pred, Var} | Acc])
      end;
    #{} ->
      %% The variable is not untagged.
      subst_phi_uses0(Left, Map, [{Pred, Var} | Acc])
  end;
subst_phi_uses0([], _Map, Acc) ->
  Acc.

-spec subst_phi_uncond(icode_phi(), icode_variable(), edge_map())
		      -> icode_phi().

subst_phi_uncond(I, Dst, Map) ->
  ArgList = subst_phi_uses_uncond0(hipe_icode:phi_arglist(I), Map, []),
  hipe_icode:mk_phi(Dst, ArgList).

-spec subst_phi_uses_uncond0([{icode_lbl(), icode_variable()}], edge_map(),
			     [{icode_lbl(), icode_variable()}])
			    -> [{icode_lbl(), icode_variable()}].

subst_phi_uses_uncond0([{Pred, Var}|Left], Map, Acc) ->
  case Map of
    #{Var := List} ->
      case lists:keyfind(Pred, 1, List) of
	{Pred, {assigned, NewVar}} ->
	  %% The variable is untagged!
	  subst_phi_uses_uncond0(Left, Map, [{Pred, NewVar} | Acc]);
	{Pred, _NewVar} = PredNV ->
	  %% The variable is untagged!
	  subst_phi_uses_uncond0(Left, Map, [PredNV | Acc]);
	false ->
	  %% The variable is not untagged.
	  subst_phi_uses_uncond0(Left, Map, [{Pred, Var} | Acc])
      end;
    #{} ->
      %% The variable is not untagged.
      subst_phi_uses_uncond0(Left, Map, [{Pred, Var} | Acc])
  end;
subst_phi_uses_uncond0([], _Map, Acc) ->
  Acc.

-spec place_error_handling(worklist(), state()) -> state().

place_error_handling(WorkList, State) ->
  case get_work(WorkList) of
    none ->
      State;
    {Label, NewWorkList} ->      
      BB = state__bb(State, Label),
      Code = hipe_bb:code(BB),
      case state__join_in_block(State, Label) of
	fixpoint ->
	  place_error_handling(NewWorkList, State);
	{NewState, NewInBlock} ->
	  {NewCode1, InBlockOut} = place_error(Code, NewInBlock, []),
	  Succ = state__succ(NewState, Label),
	  NewCode2 = handle_unchecked_end(Succ, NewCode1, InBlockOut),
	  NewBB = hipe_bb:code_update(BB, NewCode2),
	  NewState1 = state__bb_add(NewState, Label, NewBB),
	  NewState2 = state__in_block_out_update(NewState1, Label, InBlockOut),
	  NewWorkList1 = add_work(NewWorkList, Succ),
	  place_error_handling(NewWorkList1, NewState2)
      end
  end.

-spec place_error(icode_instrs(), in_block(), icode_instrs())
		 -> {icode_instrs(), in_block()}.

place_error([I|Left], InBlock, Acc) ->
  case I of
    #icode_call{} ->
      case hipe_icode:call_fun(I) of
	X when X =:= fp_add; X =:= fp_sub; 
	       X =:= fp_mul; X =:= fp_div; X =:= fnegate ->
	  case InBlock of
	    false ->
	      Clear = hipe_icode:mk_primop([], {fclearerror, []}, []),
	      place_error(Left, {true, []}, [I, Clear|Acc]);
	    {true, _} ->
	      place_error(Left, InBlock, [I|Acc])
	  end;
	unsafe_tag_float ->
	  case InBlock of
	    {true, Fail} ->
	      Check = hipe_icode:mk_primop([], fcheckerror, [], [], Fail),
	      place_error(Left, false, [I, Check|Acc]);
	    false ->
	      place_error(Left, InBlock, [I|Acc])
	  end;
	{fclearerror, Fail} ->
	  case InBlock of
	    {true, Fail} ->
	      %% We can remove this fclearerror!
	      case hipe_icode:call_continuation(I) of
		[] ->
		  place_error(Left, InBlock, Acc);
		Cont ->
		  place_error(Left, InBlock, [hipe_icode:mk_goto(Cont)|Acc])
	      end;
	    {true, _OtherFail} ->
	      %% TODO: This can be handled but it requires breaking up
	      %% the BB in two. Currently this should not happen.
	      exit("Starting fp ebb with different fail label");
	    false ->
	      place_error(Left, {true, Fail}, [I|Acc])
	  end;
	fcheckerror ->
	  case {true, hipe_icode:call_fail_label(I)} of
	    InBlock ->
	      %% No problem
	      place_error(Left, false, [I|Acc]);
	    NewInblock ->
	      exit({"Fcheckerror has the wrong fail label",
		    InBlock, NewInblock})
	  end;
	X when X =:= conv_to_float; X =:= unsafe_untag_float ->
	  place_error(Left, InBlock, [I|Acc]);
	_Other ->
	  case hipe_icode_primops:fails(hipe_icode:call_fun(I)) of
	    false ->
	      place_error(Left, InBlock, [I|Acc]);
	    true ->
	      case InBlock of
		{true, Fail} ->
		  Check = hipe_icode:mk_primop([], fcheckerror, [], [], Fail),
		  place_error(Left, false, [I, Check|Acc]);
		false ->
		  place_error(Left, InBlock, [I|Acc])
	      end
	  end
      end;
    #icode_fail{} ->
      place_error_1(I, Left, InBlock, Acc);
    #icode_return{} ->
      place_error_1(I, Left, InBlock, Acc);
    #icode_enter{} ->
      place_error_1(I, Left, InBlock, Acc);
    Other ->
      case instr_allowed_in_fp_ebb(Other) of
	true ->
	  place_error(Left, InBlock, [I|Acc]);
	false ->
	  case InBlock of
	    {true, []} ->
	      Check = hipe_icode:mk_primop([], fcheckerror, []),
	      place_error(Left, false, [I, Check|Acc]);
	    {true, _} ->
	      exit({"Illegal instruction in caught fp ebb", I});
	    false ->
	      place_error(Left, InBlock, [I|Acc])
	  end
      end
  end;
place_error([], InBlock, Acc) ->
  {lists:reverse(Acc), InBlock}.

place_error_1(I, Left, InBlock, Acc) ->
  case InBlock of
    {true, []} ->
      Check = hipe_icode:mk_primop([], fcheckerror, []),
      place_error(Left, false, [I, Check|Acc]);
    {true, _} ->
      exit({"End of control flow in caught fp ebb", I});
    false ->
      place_error(Left, InBlock, [I|Acc])
  end.

%% If the block has no successors and we still are in a fp ebb we must
%% end it to make sure we don't have any unchecked fp exceptions.

handle_unchecked_end(Succ, Code, InBlock) ->
  case Succ of
    [] ->
      case InBlock of
	{true, []} ->
	  {TopCode, Last} = split_code(Code),
	  NewI = hipe_icode:mk_primop([], fcheckerror, []),
	  TopCode ++ [NewI, Last];
	false ->
	  Code
      end;
    _ ->
      Code
  end.      

instr_allowed_in_fp_ebb(Instr) ->
  case Instr of
    #icode_comment{} -> true;
    #icode_goto{} -> true;
    #icode_if{} -> true;
    #icode_move{} -> true;
    #icode_phi{} -> true;
    #icode_begin_handler{} -> true;
    #icode_switch_tuple_arity{} -> true;
    #icode_switch_val{} -> true;
    #icode_type{} -> true;
    _ -> false
  end.

%%=============================================================
%% Help functions
%%=============================================================

%% ------------------------------------------------------------ 
%% Handling the variable map

-spec lookup_list([icode_var() | icode_const()], var_map())
		 -> [none | icode_fvar()].

lookup_list(List, Info) ->
  lookup_list(List, fun lookup/2, Info, []).

lookup_list([H|T], Fun, Info, Acc) ->
  lookup_list(T, Fun, Info, [Fun(H, Info)|Acc]);
lookup_list([], _,  _, Acc) ->
  lists:reverse(Acc).

-spec lookup(icode_var() | icode_const(), var_map()) -> none | icode_fvar().

lookup(Key, Tree) ->
  case hipe_icode:is_const(Key) of
    %% This can be true if the same constant has been
    %% untagged more than once
    true -> none;
    false ->
      case Tree of
	#{Key := {assigned, Val}} -> Val;
	#{Key := Val} -> Val;
	#{} -> none
      end
  end.

-spec lookup_list_keep_consts([icode_var() | icode_const()], var_map())
			     -> [none | icode_fvar() | icode_const()].

lookup_list_keep_consts(List, Info) ->
  lookup_list(List, fun lookup_keep_consts/2, Info, []).

-spec lookup_keep_consts(icode_var() | icode_const(), var_map())
			-> none | icode_fvar() | icode_const().

lookup_keep_consts(Key, Tree) ->
  case hipe_icode:is_const(Key) of
    true -> Key;
    false ->
      case Tree of
	#{Key := {assigned, Val}} -> Val;
	#{Key := Val} -> Val;
	#{} -> none
      end
  end.

-spec get_type(icode_argument()) -> erl_types:erl_type().

get_type(Var) ->
  case hipe_icode:is_const(Var) of
    true -> erl_types:t_from_term(hipe_icode:const_value(Var));
    false ->
      case hipe_icode:is_annotated_variable(Var) of
	true -> 
	  {type_anno, Type, _} = hipe_icode:variable_annotation(Var),
	  Type
%%%     false -> erl_types:t_any()
      end
  end.

%% ------------------------------------------------------------ 
%% Handling the map from variables to fp-variables

-spec join_maps([edge()], edge_map()) -> initial_var_map().

join_maps(Edges, EdgeMap) ->
  join_maps(Edges, EdgeMap, #{}).

join_maps([Edge = {Pred, _}|Left], EdgeMap, Map) ->
  case EdgeMap of
    #{Edge := OldMap} ->
      NewMap = join_maps0(maps:to_list(OldMap), Pred, Map),
      join_maps(Left, EdgeMap, NewMap);
    #{} ->
      %% All predecessors have not been handled. Use empty map.
      #{}
  end;
join_maps([], _, Map) ->
  Map.

-spec join_maps0(list(), icode_lbl(), initial_var_map()) -> initial_var_map().

join_maps0([{Var=#icode_variable{kind=var}, FVar}|Tail], Pred, Map) ->
  case Map of
    #{Var := List} ->
      case lists:keyfind(Pred, 1, List) of
	false ->
	  join_maps0(Tail, Pred, Map#{Var := [{Pred, FVar}|List]});
	{Pred, FVar} ->
	  %% No problem.
	  join_maps0(Tail, Pred, Map);
	_ ->
	  exit('New binding to same variable')
      end;
    #{} ->
      join_maps0(Tail, Pred, Map#{Var => [{Pred, FVar}]})
  end;
join_maps0([], _, Map) ->
  Map.

-spec filter_map(initial_var_map(), pos_integer()) -> var_map_phi().

filter_map(Map, NofPreds) ->
  filter_map(maps:to_list(Map), NofPreds, Map).

-spec filter_map([{icode_var(), incoming_fvars()}], pos_integer(),
		 var_map_phi()) -> var_map_phi().

filter_map([{Var, Bindings}|Left], NofPreds, Map) ->
  case length(Bindings) =:= NofPreds of
    true ->
      BindingsAllAssigned = lists:all(fun({_, {assigned, _}}) -> true;
					 ({_, _}) -> false
				      end, Bindings),
      case all_args_equal(Bindings) of
	true ->
	  NewBinding =
	    case hd(Bindings) of
	      {Pred, {assigned, FVar0}} when is_integer(Pred) ->
		case BindingsAllAssigned of
		  true -> {assigned, FVar0};
		  false -> FVar0
		end;
	      {Pred, FVar0} when is_integer(Pred) -> FVar0
	    end,
	  filter_map(Left, NofPreds, Map#{Var := NewBinding});
	false ->
	  PhiDst = hipe_icode:mk_new_fvar(),
	  PhiArgs = strip_of_assigned(Bindings),
	  NewMap =
	    case Map of
	      #{phi := Val} ->
		Map#{phi := [{PhiDst, PhiArgs}|Val]};
	      #{} ->
		Map#{phi => [{PhiDst, PhiArgs}]}
	    end,
	  NewBinding =
	    case BindingsAllAssigned of
	      true -> {assigned, PhiDst};
	      false -> PhiDst
	    end,
	  filter_map(Left, NofPreds, NewMap#{Var := NewBinding})
      end;
    false ->
      filter_map(Left, NofPreds, maps:remove(Var, Map))
  end;
filter_map([], _NofPreds, Map) ->
  Map.

-spec all_args_equal(incoming_fvars()) -> boolean().

%% all_args_equal returns true if the mapping for a variable is the
%% same from all predecessors, i.e., we do not need a phi-node.

%% During the fixpoint loop, a mapping might become assigned, without that
%% information having propagated into all predecessors. We take care to answer
%% true even if FVar is only assigned in some predecessors.

all_args_equal([{_, {assigned, FVar}}|Left]) ->
  all_args_equal(Left, FVar);
all_args_equal([{_, FVar}|Left]) ->
  all_args_equal(Left, FVar).

all_args_equal([{_, {assigned, FVar1}}|Left], FVar1) ->
  all_args_equal(Left, FVar1);
all_args_equal([{_, FVar1}|Left], FVar1) ->
  all_args_equal(Left, FVar1);
all_args_equal([], _) ->
  true;
all_args_equal(_, _) ->
  false.


-spec add_new_bindings_unassigned([icode_var()], var_map()) -> var_map().

%% We differentiate between values that have been assigned as
%% tagged variables and those that got a 'virtual' binding.

add_new_bindings_unassigned([Var|Left], Map) ->
  FVar = hipe_icode:mk_new_fvar(),
  add_new_bindings_unassigned(Left, Map#{Var => FVar});
add_new_bindings_unassigned([], Map) ->
  Map.

-spec add_new_bindings_assigned([icode_var()], var_map()) -> var_map().

add_new_bindings_assigned([Var|Left], Map) ->
  case lookup(Var, Map) of
    none ->
      FVar = hipe_icode:mk_new_fvar(),
      NewMap = Map#{Var => {assigned, FVar}},
      add_new_bindings_assigned(Left, NewMap);
    _ ->
      add_new_bindings_assigned(Left, Map)
  end;
add_new_bindings_assigned([], Map) ->
  Map.

-spec strip_of_assigned(incoming_fvars()) -> [{icode_lbl(), icode_fvar()}].

strip_of_assigned(List) ->
  strip_of_assigned(List, []).

strip_of_assigned([{Pred, {assigned, Val}}|Left], Acc) ->
  strip_of_assigned(Left, [{Pred, Val}|Acc]);
strip_of_assigned([Tuple|Left], Acc) ->
  strip_of_assigned(Left, [Tuple|Acc]);
strip_of_assigned([], Acc) ->
  Acc.

%% ------------------------------------------------------------ 
%% Help functions for the transformation from ordinary instruction to
%% fp-instruction

-spec is_fop_cand(icode_instr()) -> boolean().

is_fop_cand(I) ->
  case hipe_icode:call_fun(I) of
    '/' -> true;
    Fun ->
      case fun_to_fop(Fun) of
	false -> false;
	_ -> any_is_float(hipe_icode:args(I))
      end
  end.

-spec any_is_float([icode_argument()]) -> boolean().

any_is_float(Vars) ->
  lists:any(fun (V) -> erl_types:t_is_float(get_type(V)) end, Vars).

remove_duplicates(List) ->
  remove_duplicates(List, []).

remove_duplicates([X|Left], Acc) ->
  case lists:member(X, Acc) of
    true ->
      remove_duplicates(Left, Acc);
    false ->
      remove_duplicates(Left, [X|Acc])
  end;
remove_duplicates([], Acc) ->
  Acc.

fun_to_fop(Fun) ->
  case Fun of
    '+' -> fp_add;
    '-' -> fp_sub;
    '*' -> fp_mul;
    '/' -> fp_div;
    _ -> false
  end.


-spec must_be_tagged(icode_var(), var_map()) -> boolean().

%% If there is a tagged version of this variable available we don't
%% have to tag the untagged version.

must_be_tagged(Var, Map) ->
  case Map of
    #{Var := {assigned, _}} -> false;
    #{Var := Val} -> hipe_icode:is_fvar(Val);
    #{} -> false
  end.


-spec get_conv_instrs([icode_argument()], var_map()) -> icode_instrs().

%% Converting to floating point variables

get_conv_instrs(Vars, Map) ->
  get_conv_instrs(Vars, Map, []).

-spec get_conv_instrs([icode_argument()], var_map(), icode_instrs())
		     -> icode_instrs().

get_conv_instrs([Var|Left], Map, Acc) ->
  #{Var := {_, Dst}} = Map,
  NewI =
    case erl_types:t_is_float(get_type(Var)) of
      true ->
	[hipe_icode:mk_primop([Dst], unsafe_untag_float, [Var])];
      false ->
	[hipe_icode:mk_primop([Dst], conv_to_float, [Var])] 
    end,
  get_conv_instrs(Left, Map, NewI++Acc);
get_conv_instrs([], _, Acc) ->
  Acc.


-spec conv_consts([icode_const()], icode_instr()) -> icode_instr().

conv_consts(ConstArgs, I) ->
  conv_consts(ConstArgs, I, []).

conv_consts([Const|Left], I, Subst) ->
  NewConst = hipe_icode:mk_const(float(hipe_icode:const_value(Const))),
  conv_consts(Left, I, [{Const, NewConst}|Subst]);
conv_consts([], I, Subst) ->
  hipe_icode:subst_uses(Subst, I).


%% _________________________________________________________________
%%
%% Handling the state
%%

new_state(Cfg) ->
  #state{cfg = Cfg}.

state__cfg(#state{cfg = Cfg}) ->
  Cfg.

state__succ(#state{cfg = Cfg}, Label) ->
  hipe_icode_cfg:succ(Cfg, Label).

state__pred(#state{cfg = Cfg}, Label) ->
  hipe_icode_cfg:pred(Cfg, Label).

state__redirect(S = #state{cfg = Cfg}, From, ToOld, ToNew) ->
  NewCfg = hipe_icode_cfg:redirect(Cfg, From, ToOld, ToNew),
  S#state{cfg=NewCfg}.

state__bb(#state{cfg = Cfg}, Label) ->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_add(S = #state{cfg = Cfg}, Label, BB) ->
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, BB),
  S#state{cfg = NewCfg}.

-spec state__map(state(), icode_lbl()) -> initial_var_map().

state__map(S = #state{edge_map = EM}, To) ->
  join_maps([{From, To} || From <- state__pred(S, To)], EM).

-spec state__map_update(state(), icode_lbl(), icode_lbl(), var_map()) ->
			   fixpoint | state().

state__map_update(S = #state{edge_map = EM}, From, To, Map) ->
  FromTo = {From, To},
  MapChanged = 
    case EM of
      #{FromTo := Map1} -> not match(Map1, Map);
      #{} -> true
    end,
  case MapChanged of
    true ->
      NewEM = EM#{FromTo => Map},
      S#state{edge_map = NewEM};
    false ->
      fixpoint
  end.

-spec state__join_in_block(state(), icode_lbl())
			  -> fixpoint | {state(), in_block()}.

state__join_in_block(S = #state{fp_ebb_map = Map}, Label) ->
  Pred = state__pred(S, Label),
  Edges = [{X, Label} || X <- Pred],
  NewInBlock = join_in_block([maps:find(X, Map) || X <- Edges]),
  InBlockLabel = {inblock_in, Label},
  case Map of
    #{InBlockLabel := NewInBlock} ->
      fixpoint;
    _ ->
      NewMap = Map#{InBlockLabel => NewInBlock},
      {S#state{fp_ebb_map = NewMap}, NewInBlock}
  end.

-spec state__in_block_out_update(state(), icode_lbl(), in_block())
				-> state().

state__in_block_out_update(S = #state{fp_ebb_map = Map}, Label, NewInBlock) ->
  Succ = state__succ(S, Label),
  Edges = [{Label, X} || X <- Succ],
  NewMap = update_edges(Edges, NewInBlock, Map),
  NewMap1 = NewMap#{{inblock_out, Label} => NewInBlock},
  S#state{fp_ebb_map = NewMap1}.

-spec update_edges([edge()], in_block(), fp_ebb_map()) -> fp_ebb_map().

update_edges([Edge|Left], NewInBlock, Map) ->
  NewMap = Map#{Edge => NewInBlock},
  update_edges(Left, NewInBlock, NewMap);
update_edges([], _NewInBlock, NewMap) ->
  NewMap.

-spec join_in_block([error | {ok, in_block()}]) -> in_block().

join_in_block([]) ->
  false;
join_in_block([error|_]) ->
  false;
join_in_block([{ok, InBlock}|Left]) ->
  join_in_block(Left, InBlock).

-spec join_in_block([error | {ok, in_block()}], Current)
		   -> false | Current when
    Current :: in_block().

join_in_block([error|_], _Current) ->
  false;
join_in_block([{ok, InBlock}|Left], Current) ->
  if Current =:= InBlock -> join_in_block(Left, Current);
     Current =:= false -> false;
     InBlock =:= false -> false;
     true -> exit("Basic block is in two different fp ebb:s")
  end;
join_in_block([], Current) ->
  Current.


-spec state__get_in_block_in(state(), icode_lbl()) -> in_block().

state__get_in_block_in(#state{fp_ebb_map = Map}, Label) ->
  maps:get({inblock_in, Label}, Map).

state__get_in_block_out(#state{fp_ebb_map = Map}, Label) ->
  maps:get({inblock_out, Label}, Map).


-spec new_worklist(state()) -> worklist().

new_worklist(#state{cfg = Cfg}) ->
  Start = hipe_icode_cfg:start_label(Cfg),
  {[Start], [], gb_sets:insert(Start, gb_sets:empty())}.

-spec get_work(worklist()) -> none | {icode_lbl(), worklist()}.

get_work({[Label|Left], List, Set}) ->
  {Label, {Left, List, gb_sets:delete(Label, Set)}};
get_work({[], [], _Set}) ->
  none;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

-spec add_work(worklist(), [icode_lbl()]) -> worklist().

add_work({List1, List2, Set} = Work, [Label|Left]) ->
  case gb_sets:is_member(Label, Set) of
    true -> 
      add_work(Work, Left);
    false ->
      %% io:format("Added work: ~w\n", [Label]),
      NewSet = gb_sets:insert(Label, Set),
      add_work({List1, [Label|List2], NewSet}, Left)
  end;
add_work(WorkList, []) ->
  WorkList.

-spec match(var_map(), var_map()) -> boolean().

match(Tree1, Tree2) when is_map(Tree1), is_map(Tree2) ->
  Tree1 =:= Tree2.
