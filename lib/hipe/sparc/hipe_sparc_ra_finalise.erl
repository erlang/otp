%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(hipe_sparc_ra_finalise).
-export([finalise/3]).
-include("hipe_sparc.hrl").

finalise(Defun, TempMap, FPMap0) ->
  Code = hipe_sparc:defun_code(Defun),
  {_, SpillLimit} = hipe_sparc:defun_var_range(Defun),
  Map = mk_ra_map(TempMap, SpillLimit),
  FPMap1 = mk_ra_map_fp(FPMap0, SpillLimit),
  NewCode = ra_code(Code, Map, FPMap1, []),
  Defun#defun{code=NewCode}.

ra_code([I|Insns], Map, FPMap, Accum) ->
  ra_code(Insns, Map, FPMap, [ra_insn(I, Map, FPMap) | Accum]);
ra_code([], _Map, _FPMap, Accum) ->
  lists:reverse(Accum).

ra_insn(I, Map, FPMap) ->
  case I of
    #alu{} -> ra_alu(I, Map);
    #jmp{} -> ra_jmp(I, Map);
    %% #pseudo_br{} -> ra_pseudo_br(I, Map);
    #pseudo_call{} -> ra_pseudo_call(I, Map);
    #pseudo_move{} -> ra_pseudo_move(I, Map);
    #pseudo_set{} -> ra_pseudo_set(I, Map);
    #pseudo_tailcall{} -> ra_pseudo_tailcall(I, Map);
    #rdy{} -> ra_rdy(I, Map);
    #sethi{} -> ra_sethi(I, Map);
    #store{} -> ra_store(I, Map);
    #fp_binary{} -> ra_fp_binary(I, FPMap);
    #fp_unary{} -> ra_fp_unary(I, FPMap);
    #pseudo_fload{} -> ra_pseudo_fload(I, Map, FPMap);
    #pseudo_fmove{} -> ra_pseudo_fmove(I, FPMap);
    #pseudo_fstore{} -> ra_pseudo_fstore(I, Map, FPMap);
    _ -> I
  end.

ra_alu(I=#alu{src1=Src1,src2=Src2,dst=Dst}, Map) ->
  NewSrc1 = ra_temp(Src1, Map),
  NewSrc2 = ra_src(Src2, Map),
  NewDst = ra_temp(Dst, Map),
  I#alu{src1=NewSrc1,src2=NewSrc2,dst=NewDst}.

ra_jmp(I=#jmp{src1=Src1,src2=Src2}, Map) ->
  NewSrc1 = ra_temp(Src1, Map),
  NewSrc2 = ra_src(Src2, Map),
  I#jmp{src1=NewSrc1,src2=NewSrc2}.

-ifdef(notdef).	% XXX: only for sparc64, alas
ra_pseudo_br(I=#pseudo_br{src=Src}, Map) ->
  NewSrc = ra_temp(Src, Map),
  I#pseudo_br{src=NewSrc}.
-endif.

ra_pseudo_call(I=#pseudo_call{funv=FunV}, Map) ->
  NewFunV = ra_funv(FunV, Map),
  I#pseudo_call{funv=NewFunV}.

ra_pseudo_move(I=#pseudo_move{src=Src,dst=Dst}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewDst = ra_temp(Dst, Map),
  I#pseudo_move{src=NewSrc,dst=NewDst}.

ra_pseudo_set(I=#pseudo_set{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#pseudo_set{dst=NewDst}.

ra_pseudo_tailcall(I=#pseudo_tailcall{funv=FunV,stkargs=StkArgs}, Map) ->
  NewFunV = ra_funv(FunV, Map),
  NewStkArgs = ra_args(StkArgs, Map),
  I#pseudo_tailcall{funv=NewFunV,stkargs=NewStkArgs}.

ra_rdy(I=#rdy{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#rdy{dst=NewDst}.

ra_sethi(I=#sethi{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#sethi{dst=NewDst}.

ra_store(I=#store{src=Src,base=Base,disp=Disp}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewBase = ra_temp(Base, Map),
  NewDisp = ra_src(Disp, Map),
  I#store{src=NewSrc,base=NewBase,disp=NewDisp}.

ra_fp_binary(I=#fp_binary{src1=Src1,src2=Src2,dst=Dst}, FPMap) ->
  NewSrc1 = ra_temp_fp(Src1, FPMap),
  NewSrc2 = ra_temp_fp(Src2, FPMap),
  NewDst = ra_temp_fp(Dst, FPMap),
  I#fp_binary{src1=NewSrc1,src2=NewSrc2,dst=NewDst}.

ra_fp_unary(I=#fp_unary{src=Src,dst=Dst}, FPMap) ->
  NewSrc = ra_temp_fp(Src, FPMap),
  NewDst = ra_temp_fp(Dst, FPMap),
  I#fp_unary{src=NewSrc,dst=NewDst}.

ra_pseudo_fload(I=#pseudo_fload{base=Base,dst=Dst}, Map, FPMap) ->
  NewBase = ra_temp(Base, Map),
  NewDst = ra_temp_fp(Dst, FPMap),
  I#pseudo_fload{base=NewBase,dst=NewDst}.

ra_pseudo_fmove(I=#pseudo_fmove{src=Src,dst=Dst}, FPMap) ->
  NewSrc = ra_temp_fp(Src, FPMap),
  NewDst = ra_temp_fp(Dst, FPMap),
  I#pseudo_fmove{src=NewSrc,dst=NewDst}.

ra_pseudo_fstore(I=#pseudo_fstore{src=Src,base=Base}, Map, FPMap) ->
  NewSrc = ra_temp_fp(Src, FPMap),
  NewBase = ra_temp(Base, Map),
  I#pseudo_fstore{src=NewSrc,base=NewBase}.

%%% Tailcall stack arguments.

ra_args([Arg|Args], Map) ->
  [ra_temp_or_imm(Arg, Map) | ra_args(Args, Map)];
ra_args([], _) ->
  [].

ra_temp_or_imm(Arg, Map) ->
  case hipe_sparc:is_temp(Arg) of
    true ->
      ra_temp(Arg, Map);
    false ->
      Arg
  end.

%%% FunV, Src, and Temp operands.

ra_funv(FunV, Map) ->
  case FunV of
    #sparc_temp{} -> ra_temp(FunV, Map);
    _ -> FunV
  end.

ra_src(Src, Map) ->
  case Src of
    #sparc_temp{} -> ra_temp(Src, Map);
    _ -> Src
  end.

ra_temp_fp(Temp, FPMap) ->
  Reg = hipe_sparc:temp_reg(Temp),
  double = hipe_sparc:temp_type(Temp),
  case hipe_sparc_registers:is_precoloured_fpr(Reg) of
    true -> Temp;
    _ -> ra_temp_common(Reg, Temp, FPMap)
  end.

ra_temp(Temp, Map) ->
  Reg = hipe_sparc:temp_reg(Temp),
  case hipe_sparc:temp_type(Temp) of
    'double' ->
      exit({?MODULE,ra_temp,Temp});
    _ ->
      case hipe_sparc_registers:is_precoloured_gpr(Reg) of
	true -> Temp;
	_ -> ra_temp_common(Reg, Temp, Map)
      end
  end.

ra_temp_common(Reg, Temp, Map) ->
  case gb_trees:lookup(Reg, Map) of
    {value, NewReg} -> Temp#sparc_temp{reg=NewReg};
    _ -> Temp
  end.

mk_ra_map(TempMap, SpillLimit) ->
  %% Build a partial map from pseudo to reg or spill.
  %% Spills are represented as pseudos with indices above SpillLimit.
  %% (I'd prefer to use negative indices, but that breaks
  %% hipe_sparc_registers:is_precoloured/1.)
  %% The frame mapping proper is unchanged, since spills look just like
  %% ordinary (un-allocated) pseudos.
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit, is_precoloured_gpr),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      TempMap).

conv_ra_maplet(MapLet = {From,To}, SpillLimit, IsPrecoloured) ->
  %% From should be a pseudo, or a hard reg mapped to itself.
  if is_integer(From), From =< SpillLimit ->
      case hipe_sparc_registers:IsPrecoloured(From) of
	false -> [];
	_ ->
	  case To of
	    {reg, From} -> [];
	    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
	  end
      end;
     true -> exit({?MODULE,conv_ra_maplet,MapLet})
  end,
  %% end of From check
  case To of
    {reg, NewReg} ->
      %% NewReg should be a hard reg, or a pseudo mapped
      %% to itself (formals are handled this way).
      if is_integer(NewReg) ->
	  case hipe_sparc_registers:IsPrecoloured(NewReg) of
	    true -> [];
	    _ -> if From =:= NewReg -> [];
		    true ->
		     exit({?MODULE,conv_ra_maplet,MapLet})
		 end
	  end;
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of NewReg check
      {From, NewReg};
    {spill, SpillIndex} ->
      %% SpillIndex should be >= 0.
      if is_integer(SpillIndex), SpillIndex >= 0 -> [];
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of SpillIndex check
      ToTempNum = SpillLimit+SpillIndex+1,
      MaxTempNum = hipe_gensym:get_var(sparc),
      if MaxTempNum >= ToTempNum -> ok;
	 true -> hipe_gensym:set_var(sparc, ToTempNum)
      end,
      {From, ToTempNum};
    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
  end.

mk_ra_map_fp(FPMap, SpillLimit) ->
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit,
					     is_precoloured_fpr),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      FPMap).
