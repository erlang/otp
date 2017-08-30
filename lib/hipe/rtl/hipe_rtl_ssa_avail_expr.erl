%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% 
%%% %CopyrightEnd%
%%%
%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_ssa_avail_expr.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : A couple of optimizations on rtl_ssa
%%%               1. Remove unnecessary loads (Global)
%%%               2. Remove unnecessary stores (Local)
%%%               3. Remove unnecessary tag/untag operations
%%%
%%% Changed :  7 Feb 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_ssa_avail_expr).

-export([cfg/1]).

-include("../main/hipe.hrl").
-include("hipe_rtl.hrl").

cfg(CFG) ->
  CFG1 = remove_loads(CFG),
  CFG2 = remove_stores(CFG1),
  CFG3 = optimize_fixnums(CFG2),
  hipe_rtl_ssa:remove_dead_code(CFG3).

%%%=============================================================================
%%%
%%% Remove unnecessary loads
%%%
%%%=============================================================================

remove_loads(CFG) -> 
  LoadsFun = fun spread_info/2,
  Info=fix_point(CFG, LoadsFun),
  pass_through(CFG, LoadsFun, Info).

spread_info(Code, Info) ->
  lists:foldl(fun do_instr/2, {[],Info}, Code).

do_instr(Instr, {Acc,Info}) ->
  case Instr of
    #call{} ->
      {Acc++[Instr], new_env()};
    #store{} ->  
      {Acc++[Instr], new_env()};
    #gctest{} ->  
      {Acc++[Instr], new_env()};
    #load{} ->
      Dst = hipe_rtl:load_dst(Instr),
      LoadType = {hipe_rtl:load_src(Instr), hipe_rtl:load_offset(Instr),
	      hipe_rtl:load_size(Instr), hipe_rtl:load_sign(Instr)},
      NewInstr = 
	case lookup_y(LoadType, Info) of
	  none ->
	    Instr;
	  Var ->
	    hipe_rtl:mk_move(Dst, Var)
	end,
      Fun = fun load_filter_fun/2,
      {Acc++[NewInstr], insert(Dst,LoadType,remove_defines(Instr,Info,Fun))};
    _ ->
      {Acc++[Instr],remove_defines(Instr,Info,fun load_filter_fun/2)}
  end.

load_filter_fun({X1,{X2,X3,_,_}},PreColDefs) ->
  not (lists:member(X1,PreColDefs) or
       lists:member(X2,PreColDefs) or
       lists:member(X3,PreColDefs)).

%%%=============================================================================
%%%
%%% Remove unnecessary stores (local optimization)
%%%
%%%=============================================================================

remove_stores(CFG) ->
  pass_through(CFG, fun remove_store/2, new_info()).

remove_store(Code,_) ->
  remove_store_from_bb(Code).

remove_store_from_bb(Code) ->
  remove_store_from_bb(lists:reverse(Code), new_env(), []).

remove_store_from_bb([Instr|Instrs], Env, Acc) ->
  {NewAcc, NewEnv} =
    case Instr of
      #call{} ->
	{[Instr|Acc],new_env()};
      #gctest{} ->  
      {[Instr|Acc], new_env()};
      #store{} ->  
	Base = hipe_rtl:store_base(Instr),
	Offset = hipe_rtl:store_offset(Instr),
	Size = hipe_rtl:store_size(Instr),
	StoreType = {Base, Offset, Size},
	case lookup_y(StoreType, Env) of
	  none ->
	    {[Instr|Acc], insert(StoreType, true, Env)};
	  true ->
	    {Acc, Env}
	end;
      #load{} ->
	{[Instr|Acc],new_env()};
      _ -> 
	{[Instr|Acc],remove_defines(Instr,Env,fun store_filter_fun/2)}
    end,
  remove_store_from_bb(Instrs, NewEnv, NewAcc);
remove_store_from_bb([], Env, Acc) ->
  {Acc,Env}.

store_filter_fun({{X1,X2,_},_},PreColDefs) ->
  not (lists:member(X1,PreColDefs) or
       lists:member(X2,PreColDefs)).

%%%=============================================================================
%%%
%%% Optimize Fixnum Operations
%%%
%%%=============================================================================

optimize_fixnums(CFG) -> 
  FixFun = fun fixnum_opt/2,
  Info=fix_point(CFG, FixFun),
  pass_through(CFG, FixFun, Info).

fixnum_opt(Code,Info) ->
  lists:foldl(fun do_fixnums/2, {[],Info}, Code).

do_fixnums(Instr, {Acc,Env}) ->
  case Instr of
    #call{} -> 
      {Acc++[Instr],Env};
    #gctest{} ->
      {Acc++[Instr],Env};
    #fixnumop{dst=Dst,src=Src} -> 
      case lookup_y(Src,Env) of
	none -> 
	  case lookup_x(Src,Env) of
	    none ->
	      case hipe_rtl_arch:is_precoloured(Src) or 
		hipe_rtl_arch:is_precoloured(Dst) of
		true ->
		  {Acc++[Instr],Env}; %% To Avoid non ssa problems
		false ->
		  {Acc++[Instr],insert(Dst,Src,Env)}
	      end;
	    OtherSrc ->
	      {Acc++[hipe_rtl:mk_move(Dst,OtherSrc)],Env}
	  end;
	OtherDst ->
	  {Acc++[hipe_rtl:mk_move(Dst,OtherDst)],Env}
      end;
    _ ->
      {Acc++[Instr],Env}
  end.
	      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Code handling functions 
%% 

get_code_from_label(Label,CFG) ->
  CurrentBB = hipe_rtl_cfg:bb(CFG, Label),
  hipe_bb:code(CurrentBB).

put_code_at_label(Label,Code,CFG) ->
  NewBB = hipe_bb:mk_bb(Code),
  hipe_rtl_cfg:bb_add(CFG, Label, NewBB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The info environment.
%% An info environment is a mapping from labels to info_out
%%

new_info() ->
  gb_trees:empty().

get_info(Label,Info) ->
  case gb_trees:lookup(Label, Info) of
      {value, V} -> V;
      none -> none
    end.
  
add_info(Label, NewInfo, OldInfo) ->
  gb_trees:enter(Label, NewInfo, OldInfo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Simple worklist utility
%% 

add_succ_to_list(NewList, OldList) ->
  RealNew = [New || New <- NewList, lists:member(New,OldList)],
  OldList ++ RealNew.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Generic Fixpoint Code
%% 

fix_point(CFG, Fun) ->
  Start = hipe_rtl_cfg:start_label(CFG),
  Info = new_info(),
  fix_point([Start], CFG, Fun, Info).

fix_point([Label|Labels], CFG, Fun, Info) ->
  case initial_stage(Label,CFG,Fun,Info) of
    {true, _, _} ->
      fix_point(Labels, CFG, Fun, Info);
    {false, _, NewInfoOut} ->
      Succ = hipe_rtl_cfg:succ(CFG, Label), 
      NewList = add_succ_to_list(Succ, Labels),
      NewInfo = add_info(Label, NewInfoOut, Info),
      fix_point(NewList, CFG, Fun, NewInfo)
  end;
fix_point([], _CFG, _Fun, Info) ->
  Info.

pass_through(CFG, Fun, Info) ->
  pass_through(hipe_rtl_cfg:reverse_postorder(CFG),
	       CFG, Fun, Info).

pass_through([Label|Labels], CFG, Fun, Info) ->
  {_, NewCode, _} = initial_stage(Label,CFG,Fun,Info),
  NewCFG = put_code_at_label(Label,NewCode,CFG),
  pass_through(Labels, NewCFG, Fun, Info);
pass_through([], CFG, _Fun, _Info) ->
  CFG.

initial_stage(Label,CFG,Fun,Info) -> 
  OldInfoOut = get_info(Label,Info),
  Pred = hipe_rtl_cfg:pred(CFG,Label),
  InfoEnv = join([get_info(L,Info) || L <- Pred]),
  OldCode = get_code_from_label(Label,CFG),
  {PhiCode,Code} = split_code(OldCode),
  InfoIn = join_phi(PhiCode,Info,InfoEnv),
  {NewCode, NewInfoOut} = Fun(Code, InfoIn),
  {OldInfoOut=:=NewInfoOut,PhiCode++NewCode, NewInfoOut}.

join_phi([#phi{dst=Dst,arglist=AList}|Rest], Info, Env) ->
  case lists:foldl(fun(Val,Acc) ->
		       check_label(Val,Info,Acc)
		   end, none, AList) of
    no_val ->
      join_phi(Rest,Info,Env);
    none ->
      join_phi(Rest,Info,Env);
    Expr ->
      join_phi(Rest,Info,insert(Dst,Expr,Env))
  end;
join_phi([], _Info, Env) ->
  Env.
  
check_label({Lbl,Var}, Info, Acc) ->
  case gb_trees:lookup(Lbl,Info) of
    none -> Acc;
    {value,Env} -> 
      case lookup_x(Var,Env) of
	none -> no_val;
	Acc -> Acc;
	V ->
	  if Acc =:= none -> V;
	     true -> no_val
	  end
      end
  end.

split_code(Code) ->
  Phis = extract_phis(Code),
  {Phis,Code--Phis}.
	  
extract_phis(Code) ->
  [I || #phi{}=I <- Code].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% One2One Environment
%% 

new_env() ->
  {gb_trees:empty(),gb_trees:empty()}.

insert(X,Y,{XtoY,YtoX}) ->
  NewYtoX = remove_old_binding(X,XtoY,YtoX),
  NewXtoY = remove_old_binding(Y,YtoX,XtoY),
  {gb_trees:enter(X,Y,NewXtoY),
   gb_trees:enter(Y,X,NewYtoX)}.

remove_old_binding(Key,LookupTree,ChangeTree) ->
  case gb_trees:lookup(Key,LookupTree) of
    none ->
      ChangeTree;
    {value,V} ->
      gb_trees:balance(gb_trees:delete(V,ChangeTree))
  end.

lookup_x(X,{XtoY,_YtoX}) ->
  case gb_trees:lookup(X,XtoY) of
    none -> none;
    {value,Val} -> Val
  end.

lookup_y(Y,{_XtoY,YtoX}) ->
  case gb_trees:lookup(Y,YtoX) of
     none -> none;
    {value,Val} -> Val
  end.

join([]) -> new_env();
join([none]) -> new_env();
join([E]) -> E;
join([E1,E2|Rest]) -> join([join(E1,E2)|Rest]).

join({MapXY1,MapYX1},{MapXY2,MapYX2}) ->      
  {join_maps(MapXY1,MapXY2),
   join_maps(MapYX1,MapYX2)};
join(none,E) -> E;
join(E,none) -> E.
  
join_maps(Map1,Map2) ->
  OrdDict = ordsets:intersection(gb_trees:to_list(Map1),
			   gb_trees:to_list(Map2)),
  gb_trees:from_orddict(OrdDict).

remove_defines(Instr,Info,Fun) ->
  Defs = hipe_rtl:defines(Instr),
  case [Def || Def <- Defs, hipe_rtl_arch:is_precoloured(Def)] of
    [] ->
      Info;
    PreColDefs ->
      filter_environments(PreColDefs,Info,Fun)
  end.

filter_environments(PreColDefs,{M1,_M2},Fun) ->
  L1 = gb_trees:to_list(M1),
  F1 = [Tup || Tup <- L1, Fun(Tup,PreColDefs)],
  F2 = [{Y,X} || {X,Y} <- F1],
  {gb_trees:from_orddict(F1),gb_trees:from_orddict(orddict:from_list(F2))}.
