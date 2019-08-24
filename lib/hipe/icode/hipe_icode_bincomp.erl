%%% -*- erlang-indent-level: 2 -*-
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
%%%-------------------------------------------------------------------
%%% File    : hipe_icode_bincomp.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created : 12 Sep 2005 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------

-module(hipe_icode_bincomp).

-export([cfg/1]).

%%--------------------------------------------------------------------

-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").

%%--------------------------------------------------------------------

-spec cfg(cfg()) -> cfg().

cfg(Cfg1) ->
  StartLbl = hipe_icode_cfg:start_label(Cfg1),
  find_bs_get_integer([StartLbl], Cfg1, set_from_list([StartLbl])).

find_bs_get_integer([Lbl|Rest], Cfg, Visited) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  Last = hipe_bb:last(BB),
  NewCfg = 
     case ok(Last, Cfg) of
       {ok,{Type, FakeFail, RealFail, SuccLbl, MsIn, MsOut}} ->
	 {Cont, Info, OldLbl, LastMsOut} = 
	   collect_info(SuccLbl, Cfg, [Type], Lbl, RealFail, MsOut),
	 update_code(Lbl, OldLbl, Cfg, Info, Cont, FakeFail, MsIn, LastMsOut);
       not_ok ->
	 Cfg
     end,
  Succs = hipe_icode_cfg:succ(NewCfg, Lbl),
  NewSuccs = not_visited(Succs, Visited),
  NewLbls = NewSuccs ++ Rest,
  NewVisited = set_union(set_from_list(NewSuccs), Visited),
  find_bs_get_integer(NewLbls, NewCfg, NewVisited);
find_bs_get_integer([], Cfg, _) ->
  Cfg.

ok(I, Cfg) ->
  case hipe_icode:is_call(I) of
    true ->
      case hipe_icode:call_fun(I) of
	{hipe_bs_primop, {bs_get_integer, Size, Flags}} when (Flags band 6) =:= 0 ->
	  case {hipe_icode:call_dstlist(I), hipe_icode:call_args(I)} of
	    {[Dst, MsOut] = DstList, [MsIn]} ->
	      Cont = hipe_icode:call_continuation(I),
	      FirstFail = hipe_icode:call_fail_label(I),
	      FirstFailBB = hipe_icode_cfg:bb(Cfg, FirstFail),
	      case check_for_restore_block(FirstFailBB, DstList) of
		{restore_block, RealFail} ->
		  {ok, {{Dst, Size}, FirstFail, RealFail, Cont, MsIn, MsOut}};
		not_restore_block ->
		  not_ok
	      end;
	    _ ->
	      not_ok
	  end;
	_ ->
	  not_ok
      end;
    false ->
      not_ok
  end.

check_for_restore_block(FirstFailBB, DefVars) ->
  Moves = hipe_bb:butlast(FirstFailBB),
  case [Instr || Instr <- Moves, is_badinstr(Instr, DefVars)] of
    [] ->
      Last = hipe_bb:last(FirstFailBB),
      case hipe_icode:is_goto(Last) of
	true ->
	  {restore_block, hipe_icode:goto_label(Last)};
	false ->
	  not_restore_block
      end;
    [_|_] ->
      not_restore_block
  end.

is_badinstr(Instr, DefVars) ->
  not(hipe_icode:is_move(Instr) andalso
      lists:member(hipe_icode:move_dst(Instr), DefVars)).

collect_info(Lbl, Cfg, Acc, OldLbl, FailLbl, MsOut) ->
  case do_collect_info(Lbl, Cfg, Acc, FailLbl, MsOut) of
    done ->
      {Lbl, Acc, OldLbl, MsOut};
    {cont, NewAcc, NewLbl, NewMsOut} ->
      collect_info(NewLbl, Cfg, NewAcc, Lbl, FailLbl, NewMsOut)
  end.

do_collect_info(Lbl, Cfg, Acc, FailLbl, MsOut) ->
  BB = hipe_icode_cfg:bb(Cfg,Lbl),
  case hipe_bb:code(BB) of
    [I] -> 
      case hipe_icode_cfg:pred(Cfg,Lbl) of
	[_] ->
	  case ok(I, Cfg) of
	   {ok, {Type,_FakeFail,FailLbl,SuccLbl,MsOut,NewMsOut}} ->
	      NewAcc = [Type|Acc],
	      MaxSize = hipe_rtl_arch:word_size() * 8 - 5,
	      case calc_size(NewAcc) of
		Size when Size =< MaxSize ->
		  {cont,NewAcc,SuccLbl,NewMsOut};
	        _ ->
		  done
	      end;
	    _ ->
	      done
	  end;
	_ ->
	  done
      end;
    _ ->
      done
  end.
      
calc_size([{_,Size}|Rest]) when is_integer(Size) ->
  Size + calc_size(Rest);
calc_size([]) -> 0.

update_code(_Lbl, _, Cfg, [_Info], _Cont, _LastFail, _MsIn, _MsOut) ->
  Cfg;
update_code(Lbl, OldLbl, Cfg, Info, Cont, LastFail, MsIn, MsOut) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  ButLast = hipe_bb:butlast(BB),
  NewVar = hipe_icode:mk_new_var(),
  Size = calc_size(Info),
  NewLast = 
    hipe_icode:mk_primop([NewVar,MsOut],
			 {hipe_bs_primop, {bs_get_integer,Size,0}},
			 [MsIn],
			 OldLbl,
			 LastFail),
  NewBB = hipe_bb:mk_bb(ButLast++[NewLast]),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Lbl, NewBB),
  fix_rest(Info, NewVar, OldLbl, Cont, NewCfg).

fix_rest(Info, Var, Lbl, Cont, Cfg) ->
  ButLast = make_butlast(Info, Var),
  Last = hipe_icode:mk_goto(Cont),
  NewBB = hipe_bb:mk_bb(ButLast++[Last]),
  hipe_icode_cfg:bb_add(Cfg, Lbl, NewBB).

make_butlast([{Res,_Size}], Var) ->
  [hipe_icode:mk_move(Res, Var)];
make_butlast([{Res, Size}|Rest], Var) ->
  NewVar = hipe_icode:mk_new_var(),
  [hipe_icode:mk_primop([Res], 'band',
			[Var, hipe_icode:mk_const((1 bsl Size)-1)]),
   hipe_icode:mk_primop([NewVar], 'bsr', [Var, hipe_icode:mk_const(Size)])
   |make_butlast(Rest, NewVar)].

%%--------------------------------------------------------------------
%% Sets

set_from_list([]) -> #{};
set_from_list(L) ->
  maps:from_list([{E, []} || E <- L]).

not_visited([], _) -> [];
not_visited([E|T], M) ->
  case M of
    #{E := _} -> not_visited(T, M);
    _ -> [E|not_visited(T, M)]
  end.

set_union(A, B) -> maps:merge(A, B).
