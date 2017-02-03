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
%%----------------------------------------------------------------------
%% File    : hipe_icode_mulret.erl
%% Author  : Christoffer Vikström <chvi3471@it.uu.se>
%% Purpose : 
%% Created : 23 Jun 2004 by Christoffer Vikström <chvi3471@it.uu.se>
%%----------------------------------------------------------------------

-module(hipe_icode_mulret).
-export([mult_ret/4]).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").

%%>----------------------------------------------------------------------<
%% Procedure : mult_ret/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<

-spec mult_ret([_], atom(), comp_options(), _) -> [_].

mult_ret(List, Mod, Opts, Exports) ->
  case length(List) > 1 of
    true ->
      Table = analyse(List, Mod, Exports),
      %% printTable(Mod, Exports, Table),
      optimize(List, Mod, Opts, Table);
    false ->
      List
  end.

%%>-----------------------< Analysis Steps >-----------------------------<

%%>----------------------------------------------------------------------<
%% Procedure : analyse/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
analyse(List, _Mod, Exports) ->
  MaxRets = hipe_rtl_arch:nr_of_return_regs(),
  Table = mkTable(List),
  %% printTable(Mod, Exports, Table),
  Table2 = filterTable(Table, MaxRets, Exports),
  %% printTable(Mod, Exports, Table2),
  Table2.

%%>----------------------------------------------------------------------<
%% Procedure : mkTable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
mkTable(List) ->
  mkTable(List, {[], []}).

mkTable([{MFA, Icode} | List], Table) ->
  %% New Icode
  {_LMin,LMax} = hipe_icode:icode_label_range(Icode),
  hipe_gensym:set_label(icode, LMax+1),
  {_VMin,VMax} = hipe_icode:icode_var_range(Icode),
  hipe_gensym:set_var(icode, VMax+1),
  case isFunDef(MFA) of
    true ->
      mkTable(List, Table);
    false ->
      CallList = mkCallList(MFA, Icode),
      Optimizable = isOptimizable(Icode),
      NewTable = addToTable(MFA, Optimizable, CallList, Table),
      mkTable(List, NewTable)
  end;
mkTable([_|List], Table) -> mkTable(List, Table);
mkTable([], Table) -> Table.

%%>----------------------------------------------------------------------<
%% Procedure : isFunDef/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
isFunDef({_, F, _}) ->
  hd(atom_to_list(F)) =:= 45.   %% 45 is the character '-'

%%>----------------------------------------------------------------------<
%% Procedure : mkCallList/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
mkCallList(MFA, Icode) ->
  Code = hipe_icode:icode_code(Icode),
  mkCallList(Code, MFA, []).

mkCallList([#icode_call{'fun'=F, dstlist=Vars, type=local}|Code], MFA, Res) ->
  {Size, DstList} = lookForDef(Code, Vars),
  mkCallList(Code, MFA, [{callPair,MFA,{F,{matchSize,Size,DstList}}}|Res]);
mkCallList([_|Code], MFA, Res) -> mkCallList(Code, MFA, Res);
mkCallList([], _, Res) -> Res.

%%>----------------------------------------------------------------------<
%% Procedure : lookForDef/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
lookForDef([#icode_type{test={tuple,Size}, true_label=L}|Code], Vars) ->
  Code2 = skipToLabel(Code, L),
  DstLst = lookForUnElems(Code2, Vars),
  case DstLst of
    [] -> {1, Vars};
    _  ->
      DstLst2 = fixDstLst(DstLst, Size),
      {Size, DstLst2}
  end;
lookForDef([#icode_move{src=Var, dst=NewVar}|Code], [Var]) ->
  lookForDef(Code, [NewVar]);
lookForDef([#icode_label{}|_], Vars) ->
  {1, Vars};
lookForDef([I|Code], [Var] = Vars) ->
  Defs = hipe_icode:defines(I),
  case lists:member(Var, Defs) of
    true ->
      {1, Vars};
    false ->
      lookForDef(Code, Vars)
  end;
lookForDef([], Vars) -> {1, Vars}.

%%>----------------------------------------------------------------------<
%% Procedure : skipToLabel/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
skipToLabel(Code, L) ->
  case skipToLabel2(Code, L) of
    noLabel ->
      Code;
    NewCode ->
      NewCode
  end.

skipToLabel2([#icode_label{name = L}|Code],L) -> Code;
skipToLabel2([_|Code], L) -> skipToLabel2(Code, L);
skipToLabel2([], _) -> noLabel.

%%>----------------------------------------------------------------------<
%% Procedure : lookForUnElems/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
lookForUnElems(Code, Var) ->
  lookForUnElems(Code, Var, []).

lookForUnElems([#icode_call{'fun'=#unsafe_element{index=Nr}, args=Var, 
			    dstlist=[Ret]}|Code], Var, Res) ->
  lookForUnElems(Code, Var, [{Nr, Ret}|Res]);
lookForUnElems([#icode_move{dst=Var}|_], [Var], Res) -> 
  lists:flatten(Res);
lookForUnElems([#icode_call{dstlist=VarList}|_], VarList, Res) -> 
  lists:flatten(Res);
lookForUnElems([_|Code], Var, Res) -> 
  lookForUnElems(Code, Var, Res);
lookForUnElems([], _, Res) -> lists:flatten(Res).

%%>----------------------------------------------------------------------<
%% Procedure : fixDstLst/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
fixDstLst(DstLst, Size) when is_integer(Size) ->
  fixDstLst(DstLst, Size, 1, []).

fixDstLst(DstLst, Size, Cnt, Res) when Cnt =< Size ->
  case isInLst(Cnt, DstLst) of
    {true, Var} ->
      fixDstLst(DstLst, Size, Cnt+1, [Var|Res]);
    false  ->
      Var = hipe_icode:mk_var(hipe_gensym:new_var(icode)),
      fixDstLst(DstLst, Size, Cnt+1, [Var|Res])
  end;
fixDstLst(_, Size, Cnt, Res) when Cnt > Size -> lists:reverse(Res).
    
%%>----------------------------------------------------------------------<
%% Procedure : isInLst/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
isInLst(Nr, [{Nr,Var}|_]) -> {true, Var};
isInLst(Cnt, [_|DstLst]) -> isInLst(Cnt, DstLst);
isInLst(_, []) -> false.

%%>----------------------------------------------------------------------<
%% Procedure : isOptimizable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
isOptimizable(Icode) ->    
  %% Icode2 = hipe_icode:fixup_fallthroughs(Icode),
  Icode2 = hipe_icode:strip_comments(Icode),
  Cfg = hipe_icode_cfg:linear_to_cfg(Icode2),
  %% hipe_icode_cfg:pp(Cfg),
  case findReturnBlocks(Cfg) of
    noReturn ->
      {false, -1};
    BlockList ->
      processReturnBlocks(BlockList, Cfg)
  end.

%%>----------------------------------------------------------------------<
%% Procedure : findReturnBlocks/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
findReturnBlocks(IcodeCfg) ->
  Labels = hipe_icode_cfg:labels(IcodeCfg),
  case searchBlocks(Labels, IcodeCfg) of 
    [] ->
      noReturn;
    BlockList->
      BlockList
  end.

%%>----------------------------------------------------------------------<
%% Procedure : searchBlocks/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
searchBlocks(Labels, IcodeCfg) ->
  searchBlocks(Labels, IcodeCfg, []).

searchBlocks([Label|Labels], IcodeCfg, Res) ->
  Block = hipe_icode_cfg:bb(IcodeCfg, Label),
  Code = hipe_bb:code(Block),
  case searchBlockCode(Code) of
    {hasReturn, RetVar} ->
      searchBlocks(Labels, IcodeCfg, [{Label, RetVar}|Res]);
    noReturn ->
      searchBlocks(Labels, IcodeCfg, Res)
  end;
searchBlocks([], _, Res) -> Res.

%%>----------------------------------------------------------------------<
%% Procedure : searchBlockCode/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
searchBlockCode([#icode_return{vars=Vars}|_]) ->
  {hasReturn, Vars};
searchBlockCode([_|Icode]) ->
  searchBlockCode(Icode);
searchBlockCode([]) -> noReturn.

%%>----------------------------------------------------------------------<
%% Procedure : processReturnBlock/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
processReturnBlocks(Blocks, Cfg) -> 
  processReturnBlocks(Blocks, Cfg, {true, -1}, []).

processReturnBlocks([{Label, Var}|BlockList], Cfg, {Opts, Size}, TypeLst) ->
  {Opt, Type, Size2} = traverseCode(Label, Var, Cfg),
  case (Size =:= -1) orelse (Size =:= Size2) of
    true ->
      processReturnBlocks(BlockList, Cfg, 
			  {Opt andalso Opts, Size2}, [Type|TypeLst]);
    false ->
      {false, -1}
  end;
processReturnBlocks([], _, Res, TypeLst) ->
  case lists:member(icode_var, TypeLst) of
    true ->
      {_, Size} = Res,
      case Size > 1 of
	true ->
	  Res;
	false ->
	  {false, -1}
      end;
    false ->
      {false, -1}
  end.

%%>----------------------------------------------------------------------<
%% Procedure : traverseCode/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
traverseCode(Label, Var, Cfg) -> 
  traverseCode(Label, Var, Cfg, []).

traverseCode(Label, Var, Cfg, LabLst) ->
  Preds = hipe_icode_cfg:pred(Cfg, Label),
  Block = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(Block),
  case findDefine(lists:reverse(Code), Var) of
    {found, Type, NumRets} ->
      {true, Type, NumRets};
    {notFound, SrcVar} ->
      case Preds of
	[] ->
	  {false, none, -1};
	[Pred] ->
	  case lists:member(Label, LabLst) of
	    false ->
	      traverseCode(Pred, SrcVar, Cfg, [Label|LabLst]);
	    true ->
	      {false, none, -1}
	  end;
	_ ->
	  {false, none, -1}
      end
  end.

%%>----------------------------------------------------------------------<
%% Procedure : findDefine/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
findDefine([#icode_call{dstlist=Vars,'fun'=mktuple,args=Vs}|_], Vars) ->
  case length(Vs) of
    1 ->
      [{Type, _}] = Vs,
      {found, Type, 1};
    Len ->
      case lists:any(fun hipe_icode:is_var/1, Vs) of
	true ->
	  {found, icode_var, Len};
	false  ->
	  {found, icode_const, Len}
      end
  end;
findDefine([#icode_move{dst=Var, src=Src}|Code], [Var]) ->
  case hipe_icode:is_var(Src) of
    true ->
      findDefine(Code, [Src]);
    false ->
      case Src of
	#icode_const{value={flat, Value}} ->
	  case is_tuple(Value) of
	    true ->
	      {found, icode_const, tuple_size(Value)};
	    false ->
	      {found, icode_const, 1}
	  end;
	_ ->
	  findDefine(Code, [Var])
      end
  end;
findDefine([_|Code], Var) ->
  findDefine(Code, Var);
findDefine([], Var) ->
  {notFound, Var}.

%%>----------------------------------------------------------------------<
%% Procedure : addToTable/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
addToTable(MFA, Optimizable, CallList, {FunLst, CallLst}) ->
  NewFunLst = [{MFA, Optimizable}|FunLst],
  {NewFunLst, CallList ++ CallLst}.

%%>----------------------------------------------------------------------<
%% Procedure : filterTable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
filterTable({FunLst, CallLst}, MaxRets, Exports) -> 
  filterTable(FunLst, CallLst, MaxRets, Exports, {[],[]}).

filterTable([Fun|FunLst], CallLst, MaxRets, Exports, {Funs, Calls} = FCs) ->
  {MFA, {ReturnOpt, Rets}} = Fun,
  {CallOpt, CallsToKeep} = checkCalls(CallLst, MFA, Rets),
  CallsToKeep2 = removeDuplicateCalls(CallsToKeep),
  NotExported = checkExported(MFA, Exports),
  case CallOpt andalso ReturnOpt andalso (Rets =< MaxRets) andalso
    NotExported andalso (not containRecursiveCalls(CallsToKeep2, MFA)) of
    true ->
      filterTable(FunLst, CallLst, MaxRets, Exports, 
		  {[Fun|Funs], CallsToKeep2 ++ Calls});
    false ->
      filterTable(FunLst, CallLst, MaxRets, Exports, FCs)
  end;
filterTable([], _, _, _, Res) -> Res.

removeDuplicateCalls(Calls) ->
  removeDuplicateCalls(Calls, []).

removeDuplicateCalls([Call|CallsToKeep], Res) -> 
  case lists:member(Call, CallsToKeep) of
    true ->
      removeDuplicateCalls(CallsToKeep, Res);
    false ->
      removeDuplicateCalls(CallsToKeep, [Call|Res])
  end;
removeDuplicateCalls([], Res) -> lists:reverse(Res).

containRecursiveCalls([Call|Calls], Fun) ->
  {callPair, Caller, {Callee, _}} = Call,
  case (Callee =:= Fun) andalso (Caller =:= Fun) of
    true ->
      true;
    false->
      containRecursiveCalls(Calls, Fun)
  end;
containRecursiveCalls([], _) -> false.

%%>----------------------------------------------------------------------<
%% Procedure : checkCalls/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
checkCalls(CallLst, MFA, Rets) ->
  checkCalls(CallLst, MFA, Rets, [], []).

checkCalls([C = {callPair, _, {MFA, {matchSize, Rets, _}}}|CallLst], 
	   MFA, Rets, Res, Opt) ->
  checkCalls(CallLst, MFA, Rets, [C|Res], [true|Opt]);
checkCalls([{callPair, _, {MFA, {matchSize, _, _}}}|CallLst], 
	   MFA, Rets, Res, Opt) ->
  checkCalls(CallLst, MFA, Rets, Res, [false|Opt]);
checkCalls([_|CallLst], MFA, Rets, Res, Opt) ->
  checkCalls(CallLst, MFA, Rets, Res, Opt);
checkCalls([], _, _, Res, Opt) -> {combineOpts(Opt), Res}.

%%>----------------------------------------------------------------------<
%% Procedure : combineOpts/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
combineOpts([]) -> false;
combineOpts([Opt]) -> Opt;
combineOpts([Opt|Opts]) -> Opt andalso combineOpts(Opts).

%%>----------------------------------------------------------------------<
%% Procedure : checkCalls/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
checkExported({_,F,A}, [{F,A}|_]) -> false;
checkExported(MFA, [_|Exports]) -> checkExported(MFA, Exports);
checkExported(_, []) -> true.

%%>----------------------< Optimization Steps >--------------------------<

%%>----------------------------------------------------------------------<
%% Procedure : optimize/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimize(List, _Mod, Opts, Table) -> 
  {FunLst, CallLst} = Table,
  List2 = optimizeFuns(FunLst, Opts, List),
  optimizeCalls(CallLst, Opts, List2).

%%>----------------------------------------------------------------------<
%% Procedure : optimizeFuns/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimizeFuns([{Fun, _}|FunList], Opts, List) ->
  NewList = findFun(List, Fun),
  optimizeFuns(FunList, Opts, NewList);
optimizeFuns([],_,List) -> List.

findFun(List, Fun) -> findFun(List, Fun, []).
findFun([{Fun, Icode}|List], Fun, Res) ->
  NewIcode = optimizeFun(Icode),
  findFun(List, Fun, [{Fun, NewIcode}|Res]);
findFun([I|List], Fun, Res) -> findFun(List, Fun, [I|Res]);
findFun([], _, Res) -> lists:reverse(Res).


optimizeFun(Icode) ->
  {_LMin,LMax} = hipe_icode:icode_label_range(Icode),
  hipe_gensym:set_label(icode, LMax+1),
  {_VMin,VMax} = hipe_icode:icode_var_range(Icode),
  hipe_gensym:set_var(icode, VMax+1),
  %% Icode2 = hipe_icode:fixup_fallthroughs(Icode),
  Icode2 = hipe_icode:strip_comments(Icode),
  Cfg = hipe_icode_cfg:linear_to_cfg(Icode2),
  case findReturnBlocks(Cfg) of
    noReturn ->
      false;
    BlockList ->
      NewCfg = optimizeReturnBlocks(BlockList, Cfg),
      hipe_icode_cfg:cfg_to_linear(NewCfg)
  end.

optimizeReturnBlocks([Block|BlockList], Cfg) -> 
  {NewCfg, Vars} = optimizeReturnBlock(Block, Cfg),
  NewCfg2 = case Vars of
	      [_] ->
		Cfg;
	      _ ->
		{Label, _} = Block,
		updateReturnBlock(Label, Vars, NewCfg)
	    end,
  optimizeReturnBlocks(BlockList, NewCfg2);
optimizeReturnBlocks([], Cfg) -> Cfg. 

optimizeReturnBlock(Block, Cfg) -> 
  optimizeReturnBlock(Block, Cfg, []).

optimizeReturnBlock({Label,Var}, Cfg, UpdateMap) ->
  Preds = hipe_icode_cfg:pred(Cfg, Label),
  Block = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(Block),
  case optimizeDefine(Code, Var) of
    {found, NewBlockCode, Vars} ->
      NewBlock = hipe_bb:code_update(Block, NewBlockCode),
      NewCfg = resolveUpdateMap(UpdateMap, Cfg),
      {hipe_icode_cfg:bb_add(NewCfg, Label, NewBlock), Vars};
    {none, NewBlockCode, NewVar} ->
      case Preds of
	[Pred] ->
	  NewBlock = hipe_bb:code_update(Block, NewBlockCode),
	  optimizeReturnBlock({Pred,NewVar}, Cfg, 
			      [{Label, NewBlock}|UpdateMap]);
	[_|_] ->
	  {Cfg, Var}
      end;
    {none, noOpt} ->
      {Cfg, Var}
  end.

optimizeDefine(Code, Dst) ->
  optimizeDefine(lists:reverse(Code), Dst, [], []).

optimizeDefine([I|Code], Dsts, DstLst, Res) -> 
  [Ds] = Dsts,
  case isCallPrimop(I, mktuple) andalso DstLst =:= [] of
    true ->
      case hipe_icode:call_dstlist(I) =:= Dsts of
	true ->
	  case length(hipe_icode:call_args(I)) > 1 of
	    true ->
	      optimizeDefine(Code, Dsts, hipe_icode:call_args(I), Res);
	    false ->
	      {none, noOpt}
	  end;
	false ->
	  optimizeDefine(Code, Dsts, DstLst, [I|Res])
      end;
    false ->
      case hipe_icode:is_move(I) andalso DstLst =:= [] of
	true ->
	  case hipe_icode:move_dst(I) =:= Ds of
	    true ->
	      Src = hipe_icode:move_src(I),
	      case hipe_icode:is_var(Src) of
		true ->
		  NewDst = hipe_icode:move_src(I),
		  optimizeDefine(Code, [NewDst], DstLst, Res);
		false ->
		  case Src of
		    #icode_const{value={flat, T}} when is_tuple(T) ->
		      NewLst = tuple_to_list(T),
		      optimizeDefine(Code, Dsts, NewLst, Res);
		    _ ->
		      {none, noOpt}
		  end
	      end;
	    false ->
	      optimizeDefine(Code, Dsts, DstLst, [I|Res])
	  end;
	false ->
	  case lists:member(Ds, hipe_icode:defines(I)) andalso DstLst =:= [] of
	    true ->
	      {none, noOpt};
	    false ->
	      optimizeDefine(Code, Dsts, DstLst, [I|Res])
	  end
      end
  end;
optimizeDefine([], Dsts, DstLst, Res) -> 
  case DstLst of
    [] -> 
      {none, Res, Dsts};
    _  ->
      {found, Res, DstLst}
  end.

resolveUpdateMap([{Label, Block}|UpdateMap], Cfg) ->
  resolveUpdateMap(UpdateMap, hipe_icode_cfg:bb_add(Cfg, Label, Block));
resolveUpdateMap([], Cfg) -> Cfg.

%%>----------------------------------------------------------------------<
%% Procedure : updateReturnBlock/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
updateReturnBlock(Label, Vars, IcodeCfg) -> 
  Block = hipe_icode_cfg:bb(IcodeCfg, Label),
  Code = hipe_bb:code(Block),
  NewCode = updateReturnCode(Code, Vars),
  NewBlock = hipe_bb:code_update(Block, NewCode),
  hipe_icode_cfg:bb_add(IcodeCfg, Label, NewBlock).

updateReturnCode(Code, DstLst) ->
  updateReturnCode(Code, DstLst, []).

updateReturnCode([I| Code], DstLst, Res) -> 
  case hipe_icode:is_return(I) of
    true ->
      updateReturnCode(Code, DstLst, [hipe_icode:mk_return(DstLst)|Res]);
    false ->
      updateReturnCode(Code, DstLst, [I|Res])
    end;
updateReturnCode([], _, Res) -> lists:reverse(Res).  

%%>----------------------------------------------------------------------<
%% Procedure : optimizeCalls/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimizeCalls([Call|CallLst], _Opts, List) ->
  {callPair, Caller, {Callee, {matchSize, _, DstLst}}} = Call,
  NewList = optimizeCall(List, Caller, Callee, DstLst),
  optimizeCalls(CallLst, _Opts, NewList);
optimizeCalls([], _Opts, List) -> List.

%%>----------------------------------------------------------------------<
%% Procedure : optimizeCall/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimizeCall(List, Caller, Callee, DstLst) -> 
  optimizeCall(List, Caller, Callee, DstLst, []).

optimizeCall([{MFA, Icode}|List], MFA, Callee, DstLst, Res) ->
  {_LMin,LMax} = hipe_icode:icode_label_range(Icode),
  hipe_gensym:set_label(icode, LMax+1),
  {_VMin,VMax} = hipe_icode:icode_var_range(Icode),
  hipe_gensym:set_var(icode, VMax+1),
  %% Icode2 = hipe_icode:fixup_fallthroughs(Icode),
  Icode2 = hipe_icode:strip_comments(Icode),
  Cfg = hipe_icode_cfg:linear_to_cfg(Icode2),
  NewIcode = findAndUpdateCalls(Cfg, Callee, DstLst),
  optimizeCall(List, MFA, Callee, DstLst, [{MFA, NewIcode}|Res]);
optimizeCall([I|List], Caller, Callee, DstLst, Res) ->
  optimizeCall(List, Caller, Callee, DstLst, [I|Res]);
optimizeCall([], _, _, _, Res) -> lists:reverse(Res).

%%>----------------------------------------------------------------------<
%% Procedure : findAndUpdateCall/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
findAndUpdateCalls(Cfg, Callee, DstLst) ->
  Labels = hipe_icode_cfg:labels(Cfg), 
  Cfg2 = findAndUpdateCalls(Cfg, Labels, Callee, DstLst, []),
  hipe_icode_cfg:cfg_to_linear(Cfg2).
findAndUpdateCalls(Cfg, [L|Labels], Callee, DstLst, Visited) ->
  %% Block = hipe_icode_cfg:bb(Cfg, L),
  %% Code = hipe_bb:code(Block),
  case containCorrectCall(Cfg, L, Callee, DstLst) of
    true ->
      Block = hipe_icode_cfg:bb(Cfg,L),
      Code = hipe_bb:code(Block),
      {NewCode, OldVar} = updateCode(Code, Callee, DstLst),
      NewBlock = hipe_bb:code_update(Block, NewCode),
      Cfg2 = hipe_icode_cfg:bb_add(Cfg, L, NewBlock),
      Cfg3 = cleanUpAffectedCode(Cfg2, OldVar, Callee, L, Visited),
      findAndUpdateCalls(Cfg3, Labels, Callee, DstLst, [L|Visited]);
    false ->
      findAndUpdateCalls(Cfg, Labels, Callee, DstLst, [L|Visited])
  end;
findAndUpdateCalls(Cfg,[], _, _, _) -> Cfg.

containCorrectCall(Cfg, Label, Callee, DstLst) ->
  Block = hipe_icode_cfg:bb(Cfg,Label),
  Code = hipe_bb:code(Block),
  case containCallee(Code, Callee) of
    {true, OldVar} ->
      Succs = hipe_icode_cfg:succ(Cfg, Label),
      checkForUnElems(Succs, OldVar, DstLst, Cfg);
    false ->
      false
  end.

checkForUnElems([], _, _, _) -> false;
checkForUnElems([Succ|Succs], OldVar, DstLst, Cfg) ->
  Block = hipe_icode_cfg:bb(Cfg,Succ),
  Code = hipe_bb:code(Block),
  case checkForUnElems2(Code, OldVar, DstLst, []) of
    true ->
      true;
    false ->
      checkForUnElems(Succs, OldVar, DstLst, Cfg)
  end.

checkForUnElems2([I|Code], OldVar, DstLst, DstRes) ->
  case isCallPrimop(I, unsafe_element) of
    true ->
      case (hipe_icode:call_args(I) =:= OldVar) of
	true ->
	  [Dst] = hipe_icode:call_dstlist(I),
	  case lists:member(Dst, DstLst) of
	    true ->
	      checkForUnElems2(Code, OldVar, DstLst, [Dst|DstRes]);
	    false ->
	      checkForUnElems2(Code, OldVar, DstLst, DstRes)
	  end;
	false ->
	  checkForUnElems2(Code, OldVar, DstLst, DstRes)
      end;
    false ->
      checkForUnElems2(Code, OldVar, DstLst, DstRes)
  end;
checkForUnElems2([], _, DstLst, DstRes) -> DstLst =:= lists:reverse(DstRes).


containCallee([I|Code], Callee) ->
  case isCallLocal(I, Callee) of
    true ->
      {true, hipe_icode:call_dstlist(I)};
    false ->
      containCallee(Code, Callee)
  end;
containCallee([], _) -> false.


updateCode(Code, Callee, DstLst) -> 
  updateCode(Code, Callee, DstLst, [], []).

updateCode([I|Code], Callee, DstLst, Res, OldVars) ->
  case isCallLocal(I, Callee) of
    true ->
      Vars = hipe_icode:call_dstlist(I),
      I2 = hipe_icode:call_dstlist_update(I, DstLst),
      updateCode(Code, Callee, DstLst, [I2|Res], Vars);
    false ->
      updateCode(Code, Callee, DstLst, [I|Res], OldVars)
  end;
updateCode([], _, _, Res, OldVars) -> {lists:reverse(Res), OldVars}.


cleanUpAffectedCode(Cfg, OldVar, Callee, Label, Visited) ->
  Block = hipe_icode_cfg:bb(Cfg,Label),
  Code = hipe_bb:code(Block),
  {CodeBefore, CodeAfter, DstLst} = divideAtCall(Code, Callee),
  {NewCodeAfter, ContLab, FailLab} = findType(CodeAfter, OldVar),
  ContBlock = hipe_icode_cfg:bb(Cfg, ContLab),
  Succs = hipe_icode_cfg:succ(Cfg, ContLab),
  ContCode = hipe_bb:code(ContBlock),
  {NewContCode, NewFailLab} = removeUnElems(ContCode, OldVar, DstLst),
  NewBlock = hipe_bb:code_update(Block, 
				 CodeBefore ++ NewCodeAfter ++ NewContCode),
  Cfg2 = hipe_icode_cfg:bb_add(Cfg, Label, NewBlock),
  Cfg3 = resolveSuccBlocks(Succs, OldVar, DstLst, [Label|Visited],
			   NewFailLab, Cfg2),
  insertMiddleFailBlock(Cfg3, NewFailLab, FailLab, OldVar, DstLst).

divideAtCall(Code, Caller) ->
  divideAtCall(Code, Caller, []).

divideAtCall([I|Code], Caller, Tail) ->
  case isCallLocal(I, Caller) of
    true ->
      {lists:reverse([I|Tail]), Code, hipe_icode:call_dstlist(I)};
    false ->
      divideAtCall(Code, Caller, [I|Tail])
  end;
divideAtCall([], _, Tail) -> {Tail, []}.

findType(CodeAfter, OldVar) ->
  findType(CodeAfter, OldVar, [], {none, none}).

findType([I|Code], OldVar, Rest, Succs) ->
  case hipe_icode:is_type(I) of
    true ->
      case hipe_icode:type_args(I) =:= OldVar of
	true ->
	  TrueLab = hipe_icode:type_true_label(I),
	  FalseLab = hipe_icode:type_false_label(I),
	  findType(Code, OldVar, Rest, {TrueLab, FalseLab});
	false ->
	  findType(Code, OldVar, [I|Rest], Succs)
      end;
    false ->
      case hipe_icode:is_move(I) of
	true ->
	  case [hipe_icode:move_src(I)] =:= OldVar of
	    true ->
	      findType(Code, hipe_icode:move_dst(I), [I|Rest], Succs);
	    false ->
	      findType(Code, OldVar, [I|Rest], Succs)
	  end;
	false ->
	  findType(Code, OldVar, [I|Rest], Succs)
      end
  end;
findType([],_,Rest, {TrueLab, FalseLab}) ->
  {lists:reverse(Rest), TrueLab, FalseLab}.

%% Nesting hell... check for redundancies.
%% ---------------------------------------
removeUnElems(Code, OldVars, DstLst) -> 
  removeUnElems(Code, OldVars, DstLst, [], false, none).

removeUnElems([I|Code], [OldVar] = OldVars, DstLst, Res, Def, Lab)  ->
  case isCallPrimop(I, unsafe_element) of
    true ->
      case (hipe_icode:call_args(I) =:= OldVars) of
	true ->
	  removeUnElems(Code, OldVars, DstLst, Res, Def, Lab);
	false ->
	  case lists:member(OldVar, hipe_icode:call_args(I)) of
	    true ->
	      %% XXX: the following test seems redundant,
	      %% hence commented out -- KOSTIS
	      %% case Def of
	      %%	true ->
	      removeUnElems(Code, OldVars, DstLst, [I|Res], Def, Lab);
	      %%	false ->
	      %%	    removeUnElems(Code, OldVars, DstLst, 
	      %%			  [I|Res], Def, Lab)
	      %% end;
	    false ->
	      io:format("Borde aldrig kunna hamna här!", []),
	      removeUnElems(Code, OldVars, DstLst, [I|Res], Def, Lab)
	  end
      end;
    false  ->
      case hipe_icode:is_move(I) of
	true ->
	  case hipe_icode:move_src(I) =:= OldVar of
	    true ->
	      NewVar = hipe_icode:move_dst(I),
	      removeUnElems(Code, [NewVar], DstLst, [I|Res], Def, Lab);
	    false ->
	      removeUnElems(Code, OldVars, DstLst, [I|Res], Def, Lab)
	  end;
	false ->
	  case hipe_icode:is_type(I) andalso not Def of
	    true ->
	      NewFalseLab = case Lab =:= none of
			      true ->
				hipe_gensym:get_next_label(icode);
			      false ->
				Lab
			    end,
	      _I2 = updateTypeFalseLabel(I, NewFalseLab),
	      removeUnElems(Code, OldVars, DstLst, [I|Res], Def, NewFalseLab);
	    false ->
	      case lists:member(OldVar, hipe_icode:uses(I)) andalso Def of
		true ->
		  removeUnElems(Code, OldVars, DstLst, [I|Res], Def, Lab);
		false ->
		  case lists:member(OldVar, hipe_icode:defines(I)) of
		    true ->
		      removeUnElems(Code, OldVars, DstLst, [I|Res], true, Lab);
		    false ->
		      removeUnElems(Code, OldVars, DstLst, [I|Res], Def, Lab)
		  end
	      end
	  end
      end
  end;
removeUnElems([], _, _, Res,_, Lab) -> {lists:reverse(Res), Lab}.


updateTypeFalseLabel(Instr, NewFalseLabel) ->
  TrueLabel = hipe_icode:type_true_label(Instr),
  Args = hipe_icode:type_args(Instr),
  Type = hipe_icode:type_test(Instr),
  hipe_icode:mk_type(Args, Type, TrueLabel, NewFalseLabel).


resolveSuccBlocks(Succs, OldVar, DstLst, Visited, FailLab, Cfg) -> 
  NewSuccs = [X || X <- Succs, not lists:member(X, Visited)],
  resolveSuccBlocks2(NewSuccs, OldVar, DstLst, Visited, FailLab, Cfg). 

resolveSuccBlocks2([Succ|Succs], OldVar, DstLst, Vis, FailLab, Cfg) -> 
  Block = hipe_icode_cfg:bb(Cfg,Succ),
  Code = hipe_bb:code(Block),
  {NewCode, ReDefined} = checkUsesDefs(Code, OldVar, DstLst, FailLab),
  NewBlock = hipe_bb:code_update(Block, NewCode),
  Cfg2 = hipe_icode_cfg:bb_add(Cfg, Succ, NewBlock),
  case ReDefined of
    true ->
      resolveSuccBlocks2(Succs, OldVar, DstLst, [Succ|Vis], FailLab, Cfg2);
    false ->
      NewSuccs = hipe_icode_cfg:succ(Cfg, Succ),
      NewSuccs2 = [X || X <- NewSuccs, not lists:member(X, Vis++Succs)],
      resolveSuccBlocks2(NewSuccs2++Succs, OldVar, DstLst,
			 [Succ|Vis], FailLab, Cfg2)
  end;
resolveSuccBlocks2([], _, _, _, _, Cfg) -> Cfg.


checkUsesDefs(Code, OldVar, DstLst, FailLab) -> 
  checkUsesDefs(Code, OldVar, DstLst, FailLab, [], false).

checkUsesDefs([I|Code], OldVar, DstLst, FailLab, Res, Defined) ->
  [OVar] = OldVar,
  case hipe_icode:is_move(I) of
    true ->
      case hipe_icode:move_src(I) =:= OVar of
	true ->
	  NewVar = hipe_icode:move_dst(I),
	  checkUsesDefs(Code, NewVar, DstLst, FailLab, [I|Res], true);
	false ->
	  case lists:member(OVar, hipe_icode:defines(I)) of
	    true ->
	      checkUsesDefs(Code, OldVar, DstLst, FailLab, [I|Res], true);
	    false ->
	      checkUsesDefs(Code, OldVar, DstLst, FailLab, [I|Res], Defined)
	  end
      end;
    false ->
      case hipe_icode:is_type(I) andalso not Defined of
	true ->
	  case FailLab =/= none of
	    true ->
	      _I2 = updateTypeFalseLabel(I, FailLab),
	      checkUsesDefs(Code, OldVar, DstLst, FailLab, [I|Res], Defined);
	    false ->
	      checkUsesDefs(Code, OldVar, DstLst, FailLab, [I|Res], Defined)
	  end;
	false ->
	  case (lists:member(OVar, hipe_icode:uses(I))) andalso 
	    (not Defined) andalso (FailLab =/= none) of
	    true ->
	      Tpl = hipe_icode:mk_primop(OldVar, mktuple, DstLst), 
	      checkUsesDefs(Code, OldVar, DstLst, FailLab, [I,Tpl|Res], true);
	    false ->
	      case lists:member(OVar, hipe_icode:defines(I)) of
		true ->
		  checkUsesDefs(Code, OldVar, DstLst, FailLab, [I|Res], true);
		false ->
		  checkUsesDefs(Code, OldVar, DstLst, FailLab, [I|Res],Defined)
	      end
	  end
      end
  end;
checkUsesDefs([], _, _, _, Res, Defined) -> {lists:reverse(Res), Defined}.


insertMiddleFailBlock(Cfg, NewFailLabel, OldFailLabel, OldVar, DstLst) ->
  case NewFailLabel =:= none of
    true ->
      Cfg;
    false ->
      NewCode = [hipe_icode:mk_primop(OldVar, mktuple, DstLst), 
		 hipe_icode:mk_goto(OldFailLabel)],
      NewBlock = hipe_bb:mk_bb(NewCode),
      hipe_icode_cfg:bb_add(Cfg, NewFailLabel, NewBlock)
  end.


isCallLocal(Instr, Fun) ->
  hipe_icode:is_call(Instr) andalso (hipe_icode:call_type(Instr) =:= local)
    andalso (hipe_icode:call_fun(Instr) =:= Fun).

isCallPrimop(Instr, Fun) ->
  case hipe_icode:is_call(Instr) of
    true ->
      case is_tuple(hipe_icode:call_fun(Instr)) of
	true ->
	  ((hipe_icode:call_type(Instr) =:= primop) andalso
	   (element(1,hipe_icode:call_fun(Instr)) =:= Fun));
	false ->
	  ((hipe_icode:call_type(Instr) =:= primop) andalso
	   (hipe_icode:call_fun(Instr) =:= Fun))
      end;
    false ->
      false
  end.


%% >-------------------------< Debug code >------------------------------<

-ifdef(DEBUG_MULRET).

%%>----------------------------------------------------------------------<
%% Procedure : printTable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
printTable(Mod, Exports, {FunLst, CallLst}) ->
  {Y,Mo,D} = date(),
  {H,Mi,S} = time(),
  io:format("Module: ~w - (~w/~w-~w, ~w:~w:~w)~n=======~n", 
	    [Mod,D,Mo,Y,H,Mi,S]),
  io:format("Exports: ~w~n", [Exports]), 
  io:format("FunList: ~n"),
  printFunList(FunLst),
  io:format("CallList: ~n"),
  printCallList(CallLst).

printFunList([Fun|FunLst]) ->
  io:format("       ~w~n", [Fun]),
  printFunList(FunLst);
printFunList([]) -> io:format("~n").

printCallList([Call|CallLst]) ->
  io:format("       ~w~n", [Call]),
  printCallList(CallLst);
printCallList([]) -> io:format("~n").

-endif.

%% >----------------------------< Old code >--------------------------------<

%% %%>----------------------------------------------------------------------<
%% %  Procedure : findCallCode/3
%% %  Purpose   : 
%% %  Arguments : 
%% %  Return    : 
%% %  Notes     : 
%% %%>----------------------------------------------------------------------<
%% findCallCode(List, Callee, DstLst) -> findCallCode(List, Callee, DstLst, []).
%% findCallCode([I=#icode_call{'fun'=Callee, dstlist=Var, type=local}, I2, I3|List], 
%% 	     Callee, DstLst, Res) ->
%%     NewList = removeUnElems(List, Var),
%%     %% _Uses = checkForUses(NewList, Var, DstLst),
%%     Size = length(DstLst),
%%     case I2 of
%% 	#icode_type{test={tuple, Size}, args=Var, true_label=Label} ->
%% 	    case I3 of
%% 		#icode_label{name=Label} ->
%% 		    findCallCode(NewList, Callee, DstLst, 
%% 				 [I#icode_call{dstlist=DstLst}|Res]);
%% 		_ ->
%% 		    findCallCode(NewList, Callee, DstLst, 
%% 				 [#goto{label=Label}, 
%% 				  I#icode_call{dstlist=DstLst}|Res])
%% 	    end;
%% 	_ ->
%% 	    findCallCode(NewList, Callee, DstLst, 
%% 			 [I2,I#icode_call{dstlist=DstLst}|Res])
%%     end;
%% findCallCode([I|List], Callee, DstLst, Res) ->
%%     findCallCode(List, Callee, DstLst, [I|Res]);
%% findCallCode([], _, _, Res) -> lists:reverse(Res).


%% %%>----------------------------------------------------------------------<
%% %  Procedure : checkForUses
%% %  Purpose   : 
%% %  Arguments : 
%% %  Return    : 
%% %  Notes     : 
%% %%>----------------------------------------------------------------------<
%% checkForUses(List, Var, Dsts) -> checkForUses(List, Var, Dsts, [], List).
%% checkForUses([I|List], Var, Dsts, Rest, Code) ->
%%     Defs = hipe_icode:defines(I),
%%     Uses = hipe_icode:uses(I),
%%     case lists:member(Var, Uses) of
%% 	true ->
%% 	    true;
%% 	false ->
%% 	    case lists:member(Var, Defs) of
%% 		true ->
%% 		    false;
%% 		false ->
%% 		    case hipe_icode:is_branch(I) of
%% 			true ->
%% 			    Succs = hipe_icode:successors(I),
%% 			    checkSuccsForUses(Succs, Var, Dsts, Rest, Code);
%% 			false ->
%% 			    checkForUses(List, Var, Dsts, [I|Rest], Code)
%% 		    end
%% 	    end
%%     end;
%% checkForUses([], _, _, _, _) -> false.

%% checkSuccsForUses(Succs, Var, Dsts, Rest, Code) -> 
%%     checkSuccsForUses(Succs, Var, Dsts, Rest, Code, false).
%% checkSuccsForUses([S|Succs], Var, Dsts, Rest, Code, Res) ->
%%     List = gotoLabel(S, Code),
%%     Used = checkForUses(List, Var, Dsts, Rest, Code),
%%     checkSuccsForUses(Succs, Var, Code, Dsts, Used andalso Res);
%% checkSuccsForUses([], _, _, _, _, Res) -> Res.

%% gotoLabel(L, [L|List]) -> List;
%% gotoLabel(L, [_|List]) -> gotoLabel(L, List);
%% gotoLabel(_, []) -> [].


%% %%>----------------------------------------------------------------------<
%% %  Procedure : removeUnElems/2
%% %  Purpose   : 
%% %  Arguments : 
%% %  Return    : 
%% %  Notes     : Fixa så att funktionen använder defines(I) istället och
%% %              selektorer istället för att matcha på #call{}. Lätt gjort.
%% %%>----------------------------------------------------------------------<
%% removeUnElems(List, Var) -> removeUnElems(List, Var, []).
%% removeUnElems([#icode_call{'fun'={unsafe_element,_}, args=Var}|List], Var, Res) ->
%%     removeUnElems(List, Var, Res);
%% removeUnElems([I=#icode_move{dst=Var}|List], [Var], Res) ->
%%     lists:reverse(Res, [I|List]);
%% removeUnElems([I=#icode_call{dstlist=Var}|List], Var, Res) ->
%%     lists:reverse(Res, [I|List]);
%% removeUnElems([I|List], Var, Res) ->
%%     removeUnElems(List, Var, [I|Res]);
%% removeUnElems([], _, Res) -> lists:reverse(Res).

%% removeUnElems(List, Var) -> removeUnElems(List, Var, []).
%% removeUnElems([I|List], Var, Res) ->
%%     Defs = hipe_icode:defines(I),
%%     case hipe_icode:is_call(I) of
%% 	true ->
%% 	    Fn = hipe_icode:call_fun(I),
%% 	    case (hipe_icode:call_args(I) =:= Var) andalso is_tuple(Fn) of
%% 		true ->
%% 		    case element(1,Fn) =:= unsafe_element of 
%% 			true ->
%% 			    removeUnElems(List, Var, Res);
%% 			false ->
%% 			    case lists:member(Var, Defs) of
%% 				true ->
%% 				    lists:reverse(Res, [I|List]);
%% 				false ->
%% 				    removeUnElems(List, Var, [I|Res])
%% 			    end 
%% 		    end;
%% 		false ->
%% 		    case lists:member(Var, Defs) of
%% 			true ->
%% 			    lists:reverse(Res, [I|List]);
%% 			false ->
%% 			    removeUnElems(List, Var, [I|Res])
%% 		    end
%% 	    end;
%% 	false ->
%% 	    case lists:member(Var, Defs) of
%% 		true ->
%% 		    lists:reverse(Res, [I|List]);
%% 		false ->
%% 		    removeUnElems(List, Var, [I|Res])
%% 	    end
%%     end;
%% removeUnElems([], _, Res) -> lists:reverse(Res).
    

%% Old findDefine that also could update it.
%% -----------------------------------------
%% findDefine(Code, Var) -> findDefine(Code, Var, [], []).
%% findDefine([#icode_call{dstlist=Var,'fun'=mktuple,args=Vs}|Code],Var,NewCode,_)-> 
%%     findDefine(Code, Var, NewCode, Vs);
%% findDefine([I=#icode_move{dst=Var, src=Src}|Code], [Var], NewCode, _) ->
%%     case Src of
%% 	#icode_var{} ->
%% 	    findDefine(Code, [Src], [I|NewCode], [Src]);
%% 	#icode_const{value={flat, Tuple}} ->
%% 	    findDefine(Code, [Var], [I|NewCode], []) %% Check this case! [Var]
%%     end;
%% findDefine([I|Code], Var, NewCode, Vars) ->
%%     findDefine(Code, Var, [I|NewCode], Vars);
%% findDefine([], _, NewCode, Vars) ->
%%     case Vars of
%% 	[] ->
%% 	    notFound;
%% 	[_] ->
%% 	    {notFound, Vars};
%% 	_ ->
%% 	    {found, lists:reverse(NewCode), Vars}
%%     end.
    
%% modifyCode(Code, Var) ->
%%     [#icode_return{vars=Var}|Code2] = lists:reverse(Code),
%%     case (length(Var) =< hipe_rtl_arch:nr_of_return_regs()) of
%% 	true ->
%% 	    {Arity, Code3} = modifyCode(Code2, Var, []),
%% 	    {Arity, Code3};
%% 	false ->
%% 	    {1, Code}
%%     end.

%% modifyCode([I|Code], Var, Res) ->
%%     case scanInstr(I, Var) of
%% 	{move, Arity, VarLst} ->
%% 	    Code2 = [#icode_return{vars=VarLst}, I |lists:reverse(Res, Code)],
%% 	    {Arity, lists:reverse(Code2)};
%% 	{mktuple, Arity, VarLst} ->
%% 	    Code2 = [#icode_return{vars=VarLst}|lists:reverse(Res, Code)],
%% 	    {Arity, lists:reverse(Code2)};
%% 	other ->
%% 	    modifyCode(Code, Var, [I|Res])
%%     end;
%% modifyCode([], Var, Res) ->
%%     {1, lists:reverse(Res, [#icode_return{vars=Var}]}.
    
%% scanInstr(#icode_call{dstlist=Var, 'fun'=mktuple, args=Lst}, Var) ->
%%     {mktuple, length(Lst), Lst};
%% scanInstr(_, _) -> other.

%% printCode(Cfg) ->
%%     Labels = hipe_icode_cfg:labels(Cfg),
%%     {_,_,{_,F,_,_,_,_,_,_},_} = Cfg,
%%     io:format("~nFunction: ~w~n", [F]),
%%     Print = fun(Label) ->
%% 		    Block = hipe_icode_cfg:bb(Cfg, Label),
%% 		    Code = hipe_bb:code(Block),
%% 		    io:format("Label: ~w~n", [Label]),
%% 		    lists:foreach(fun(I) -> io:format("~w~n", [I]) end, Code),
%% 		    io:format("~n")
%% 	    end,
%%     lists:foreach(Print, Labels).
				
%% printList(File, [{MFA, #icode{code=Code, params=Parms}}|List]) ->
%%     io:format(File, "MFA: ~w - Params: ~w~n", [MFA, Parms]), 
%%     printList2(File, Code),
%%     printList(File, List);
%% printList(_, []) -> ok.

%% printList2(File, []) -> io:format(File, "~n~n", []);
%% printList2(File, IList) when is_list(IList) ->  
%%     [I|List] = IList,
%%     io:format(File, "~w~n", [I]),
%%     printList2(File, List);
%% printList2(File, SomethingElse) -> 
%%     io:format(File, "Got: ~w~n", [SomethingElse]).

%% optimizeDefine([#icode_call{dstlist=Var,'fun'=mktuple,args=Vs}|Code], 
%% 	       Var, _, Res) ->
%%     case Vs of
%% 	[_] ->
%% 	    {none, noOpt};
%%     	_ ->
%% 	    optimizeDefine(Code, Var, Vs, Res)
%%     end;
%% optimizeDefine([I=#icode_move{dst=Var, src=Src}|Code], [Var], Rets, Res) ->
%%     case hipe_icode:is_var(Src) of
%% 	true ->
%% 	    optimizeDefine(Code, [Src], Rets, Res);
%%      false ->
%%          case Src of
%% 	      #icode_const{value={flat, Tuple}} when is_tuple(Tuple) ->
%% 	          optimizeDefine(Code, [Var], tuple_to_list(Tuple), [I|Res]);
%% 	      #icode_const{value={flat, _}} ->
%% 	          {none, noOpt};
%% 	      _ ->
%% 	          optimizeDefine(Code, [Var], Rets, [I|Res])
%%          end
%%     end;
%% optimizeDefine([I|Code], Var, Rets, Res) ->
%%     optimizeDefine(Code, Var, Rets, [I|Res]);
%% optimizeDefine([], Var, Rets, Res) ->
%%     case Rets of
%% 	[] ->
%% 	    {none, Res, Var};
%% 	_ ->
%% 	    {found, Res, Rets}
%%     end.
