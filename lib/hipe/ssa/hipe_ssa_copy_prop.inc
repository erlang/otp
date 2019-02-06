%%% -*- Erlang -*-
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
%%% File        : hipe_ssa_copy_prop.inc
%%% Author      : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : Copy propagation on SSA form.
%%%
%%% Created     :  4 Apr 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-export([cfg/1]).

%%--------------------------------------------------------------------
%% Two passes through the code visiting the blocks in reverse
%% postorder. The first pass binds all destinations of copying moves
%% to the sources, and the second propagates the copies and removes 
%% the copying moves. 
%%
%% Problem:
%% Since phi-nodes are implemented as instructions they are not
%% atomic. If we are not careful we can get the situation (after propagation):
%%
%% v0 = phi(v0, v2)
%% v1 = phi(v0, v3)
%%          ^^
%% where the underlined v0 really corresponds to the v0 before the first 
%% phi-instruction.
%%
%% Solution: 
%%  * Find all dependencies between the uses of a phi-instruction to
%%    the destination of any earlier phi-instruction in the same phi-node; 
%%  * Keep the copying move that defines the variable used in the 
%%    latter phi-instruction; and 
%%  * Do not propagate the copy into the phi-instruction
%%
%%--------------------------------------------------------------------

-spec cfg(#cfg{}) -> #cfg{}.

cfg(Cfg) ->
  Labels = ?cfg:reverse_postorder(Cfg),
  {Info,PhiDep} = analyse(Labels, Cfg, gb_trees:empty(), gb_trees:empty()),
  rewrite(Labels, Cfg, Info, PhiDep).

analyse([Label|Left], Cfg, Info, PhiDep) ->
  BB = ?cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewPhiDep = get_phi_dep(Code, gb_sets:empty(), PhiDep),
  NewInfo = analyse_code(Code, Info),
  analyse(Left, Cfg, NewInfo, NewPhiDep);
analyse([], _Cfg, Info, PhiDep) ->
  {Info,PhiDep}.

get_phi_dep([I|Left], Defined, Dep) ->
  case ?code:is_phi(I) of
    true ->
      Use = ?code:uses(I),
      [Def] = ?code:defines(I),
      NewDep = add_dep(Use, Defined, Dep),
      get_phi_dep(Left, gb_sets:insert(Def, Defined), NewDep);
    false ->
      Dep
  end;
get_phi_dep([], _Defined, Dep) ->
  Dep.

add_dep([Use|Left], Defined, Dep) ->
  case gb_trees:lookup(Use, Dep) of
    none ->
      add_dep(Left, Defined, gb_trees:insert(Use, Defined, Dep));
    {value, Set} ->
      NewSet = gb_sets:union(Defined, Set),
      add_dep(Left, Defined, gb_trees:enter(Use, NewSet, Dep))
  end;
add_dep([], _Defined, Dep) ->
  Dep.

has_dep(Use, Def, Dep) ->
  case gb_trees:lookup(Use, Dep) of
    none ->
      false;
    {value, Set} ->
      gb_sets:is_member(Def, Set)
  end.

analyse_code([I|Left], Info) ->
  case ?code:is_move(I) of
    true ->
      NewInfo = get_info_move(I, Info),
      analyse_code(Left, NewInfo);
    false ->
      analyse_code(Left, Info)
  end;
analyse_code([], Info) ->
  Info.

get_info_move(I, Info) ->
  case ?code:uses(I) of
    [] -> %% Constant.
      Info;
    [Src] ->
      add_binding(?code:defines(I), Src, Info)
  end.

rewrite([Label|Left], Cfg, Info, PhiDep) ->
  BB = ?cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = rewrite_code(Code, Info, PhiDep, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  rewrite(Left, ?cfg:bb_add(Cfg, Label, NewBB), Info, PhiDep);
rewrite([], Cfg, _Info, _PhiDep) ->
  Cfg.

rewrite_code([I|Left], Info, PhiDep, Acc) ->
  case ?code:is_move(I) of
    true ->
      Fun = fun(X, Y) -> ?code:mk_move(X, Y) end,
      NewI = rewrite_move(I, Fun, Info, PhiDep),
      rewrite_code(Left, Info, PhiDep, NewI++Acc);
    false ->      
      NewI = rewrite_instr(I, Info, PhiDep),
      rewrite_code(Left, Info, PhiDep, [NewI|Acc])
  end;
rewrite_code([], _Info, _PhiDep, Acc) ->
  lists:reverse(Acc).

rewrite_move(I, Fun, Info, PhiDep) ->
  case ?code:uses(I) of
    [] ->%% Constant move. Keep it!
      [I];
    _ ->
      Dst = hd(?code:defines(I)),
      case gb_trees:lookup(Dst, Info) of
	{value, Root} -> 
	  case has_dep(Dst, Root, PhiDep) of
	    true -> %% Must keep the copying move!
	      [Fun(Dst, Root)];
	    false -> 
	      []
	  end;
	none -> 
	  []
      end
  end.

rewrite_instr(I, Info, PhiDep) ->
  rewrite_instr0(I, ?code:uses(I), Info, PhiDep, []).

rewrite_instr0(I, [Key|Left], Info, PhiDep, UpdateInfo) ->
  case gb_trees:lookup(Key, Info) of
    none ->
      rewrite_instr0(I, Left, Info, PhiDep, UpdateInfo);
    {value, Root} -> 
      case gb_trees:lookup(Key, Info) of
	{value, Root} -> 
	  case has_dep(Key, Root, PhiDep) of
	    true -> %% Must keep Key!
	      rewrite_instr0(I, Left, Info, PhiDep, UpdateInfo);
	    false ->
	      rewrite_instr0(I, Left, Info, PhiDep, [{Key, Root}|UpdateInfo])
	  end;
	_ ->
	  rewrite_instr0(I, Left, Info, PhiDep, UpdateInfo)
      end
  end;
rewrite_instr0(I, [], _Info, _PhiDep, UpdateInfo) ->
  ?code:subst(UpdateInfo, I).

add_binding([Key|Left], Val, Info) ->
  %% Make sure the key is bound to the end of any copy-chains.
  NewInfo = 
    case gb_trees:lookup(Val, Info) of
      {value, NewVal} ->
	gb_trees:insert(Key, NewVal, Info);
      none ->
	gb_trees:insert(Key, Val, Info)
    end,
  add_binding(Left, Val, NewInfo);
add_binding([], _, Info) ->
  Info.
