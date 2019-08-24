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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%@doc
%%	                BASIC BLOCK WEIGHTING
%%
%% Computes basic block weights by using branch probabilities as weights in a
%% linear equation system, that is then solved using Gauss-Jordan Elimination.
%%
%% The equation system representation is intentionally sparse, since most blocks
%% have at most two successors.
-module(hipe_bb_weights).
-export([compute/3, compute_fast/3, weight/2, call_exn_pred/0]).
-export_type([bb_weights/0]).

-compile(inline).

%%-define(DO_ASSERT,1).
%%-define(DEBUG,1).
-include("../main/hipe.hrl").

%% If the equation system is large, it might take too long to solve it exactly.
%% Thus, if there are more than ?HEUR_MAX_SOLVE labels, we use the iterative
%% approximation.
-define(HEUR_MAX_SOLVE, 10000).

-opaque bb_weights() :: #{label() => float()}.

-type cfg() :: any().
-type target_module() :: module().
-type target_context() :: any().
-type target() :: {target_module(), target_context()}.

-type label()            :: integer().
-type var()              :: label().
-type assignment()       :: {var(), float()}.
-type eq_assoc()         :: [{var(), key()}].
-type solution()         :: [assignment()].

%% Constant. Predicted probability of a call resulting in an exception.
-spec call_exn_pred() -> float().
call_exn_pred() -> 0.01.

-spec compute(cfg(), target_module(), target_context()) -> bb_weights().
compute(CFG, TgtMod, TgtCtx) ->
  Target = {TgtMod, TgtCtx},
  Labels = labels(CFG, Target),
  if length(Labels) > ?HEUR_MAX_SOLVE ->
      ?debug_msg("~w: Too many labels (~w), approximating.~n",
		 [?MODULE, length(Labels)]),
      compute_fast(CFG, TgtMod, TgtCtx);
     true ->
      {EqSys, EqAssoc} = build_eq_system(CFG, Labels, Target),
      case solve(EqSys, EqAssoc) of
	{ok, Solution} ->
	  maps:from_list(Solution)
      end
  end.

-spec build_eq_system(cfg(), [label()], target()) -> {eq_system(), eq_assoc()}.
build_eq_system(CFG, Labels, Target) ->
  StartLb = hipe_gen_cfg:start_label(CFG),
  EQS0 = eqs_new(),
  {EQS1, Assoc} = build_eq_system(Labels, CFG, Target, [], EQS0),
  {StartLb, StartKey} = lists:keyfind(StartLb, 1, Assoc),
  StartRow0 = eqs_get(StartKey, EQS1),
  StartRow = row_set_const(-1.0, StartRow0), % -1.0 since StartLb coef is -1.0
  EQS = eqs_put(StartKey, StartRow, EQS1),
  {EQS, Assoc}.

build_eq_system([], _CFG, _Target, Map, EQS) -> {EQS, lists:reverse(Map)};
build_eq_system([L|Ls], CFG, Target, Map, EQS0) ->
  PredProb = pred_prob(L, CFG, Target),
  {Key, EQS} = eqs_insert(row_new([{L, -1.0}|PredProb], 0.0), EQS0),
  build_eq_system(Ls, CFG, Target, [{L, Key}|Map], EQS).

pred_prob(L, CFG, Target) ->
  [begin
     BB = bb(CFG, Pred, Target),
     Ps = branch_preds(hipe_bb:last(BB), Target),
     ?ASSERT(length(lists:ukeysort(1, Ps))
	     =:= length(hipe_gen_cfg:succ(CFG, Pred))),
     case lists:keyfind(L, 1, Ps) of
       {L, Prob} when is_float(Prob) -> {Pred, Prob}
     end
   end || Pred <- hipe_gen_cfg:pred(CFG, L)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec triangelise(eq_system(), eq_assoc()) -> {eq_system(), eq_assoc()}.
triangelise(EQS, VKs) ->
  triangelise_1(mk_triix(EQS, VKs), []).

triangelise_1(TIX0, Acc) ->
  case triix_is_empty(TIX0) of
    true -> {triix_eqs(TIX0), lists:reverse(Acc)};
    false ->
      {V,Key,TIX1} = triix_pop_smallest(TIX0),
      Row0 = triix_get(Key, TIX1),
      case row_get(V, Row0) of
	Coef when Coef > -0.0001, Coef < 0.0001 ->
	  throw(error);
	_ ->
	  Row = row_normalise(V, Row0),
	  TIX2 = triix_put(Key, Row, TIX1),
	  TIX = eliminate_triix(V, Key, Row, TIX2),
	  triangelise_1(TIX, [{V,Key}|Acc])
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Triangelisation maintains its own index, outside of eqs. This index is
%% essentially a BST (used as a heap) of all equations by size, with {Key,Var}
%% as the values and only containing a subset of all the keys in the whole
%% equation system. The key operation is triix_pop_smallest/1, which pops a
%% {Key,Var} from the heap corresponding to one of the smallest equations. This
%% is critical in order to prevent the equations from growing during
%% triangelisation, which would make the algorithm O(n^2) in the common case.
-type tri_eq_system() :: {eq_system(),
			  gb_trees:tree(non_neg_integer(),
					gb_trees:tree(key(), var()))}.

triix_eqs({EQS, _}) -> EQS.
triix_get(Key, {EQS, _}) -> eqs_get(Key, EQS).
triix_is_empty({_, Tree}) -> gb_trees:is_empty(Tree).
triix_lookup(V, {EQS, _}) -> eqs_lookup(V, EQS).

mk_triix(EQS, VKs) ->
  {EQS,
   lists:foldl(fun({V,Key}, Tree) ->
		   Size = row_size(eqs_get(Key, EQS)),
		   sitree_insert(Size, Key, V, Tree)
	       end, gb_trees:empty(), VKs)}.

sitree_insert(Size, Key, V, SiTree) ->
  SubTree1 =
    case gb_trees:lookup(Size, SiTree) of
      none -> gb_trees:empty();
      {value, SubTree0} -> SubTree0
    end,
  SubTree = gb_trees:insert(Key, V, SubTree1),
  gb_trees:enter(Size, SubTree, SiTree).

sitree_update_subtree(Size, SubTree, SiTree) ->
  case gb_trees:is_empty(SubTree) of
    true -> gb_trees:delete(Size, SiTree);
    false -> gb_trees:update(Size, SubTree, SiTree)
  end.

triix_put(Key, Row, {EQS, Tree0}) ->
  OldSize = row_size(eqs_get(Key, EQS)),
  case row_size(Row) of
    OldSize -> {eqs_put(Key, Row, EQS), Tree0};
    Size ->
      Tree =
	case gb_trees:lookup(OldSize, Tree0) of
	  none -> Tree0;
	  {value, SubTree0} ->
	    case gb_trees:lookup(Key, SubTree0) of
	      none -> Tree0;
	      {value, V} ->
		SubTree = gb_trees:delete(Key, SubTree0),
		Tree1 = sitree_update_subtree(OldSize, SubTree, Tree0),
		sitree_insert(Size, Key, V, Tree1)
	    end
	end,
      {eqs_put(Key, Row, EQS), Tree}
  end.

triix_pop_smallest({EQS, Tree}) ->
  {Size, SubTree0} = gb_trees:smallest(Tree),
  {Key, V, SubTree} = gb_trees:take_smallest(SubTree0),
  {V, Key, {EQS, sitree_update_subtree(Size, SubTree, Tree)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

row_normalise(Var, Row) ->
  %% Normalise v's coef to 1.0
  %% row_set_coef ensures the coef is exactly 1.0 (no rounding errors)
  row_set_coef(Var, 1.0, row_scale(Row, 1.0/row_get(Var, Row))).

%% Precondition: Row must be normalised; i.e. Vars coef must be 1.0 (mod
%% rounding errors)
-spec eliminate(var(), key(), row(), eq_system()) -> eq_system().
eliminate(Var, Key, Row, TIX0) ->
  eliminate_abstr(Var, Key, Row, TIX0,
		  fun eqs_get/2, fun eqs_lookup/2, fun eqs_put/3).

-spec eliminate_triix(var(), key(), row(), tri_eq_system()) -> tri_eq_system().
eliminate_triix(Var, Key, Row, TIX0) ->
  eliminate_abstr(Var, Key, Row, TIX0,
		  fun triix_get/2, fun triix_lookup/2, fun triix_put/3).

%% The same function implemented for two data types, eqs and triix.
-compile({inline, eliminate_abstr/7}).
-spec eliminate_abstr(var(), key(), row(), ADT, fun((key(), ADT) -> row()),
		      fun((var(), ADT) -> [key()]),
		      fun((key(), row(), ADT) -> ADT)) -> ADT.
eliminate_abstr(Var, Key, Row, ADT0, GetFun, LookupFun, PutFun) ->
  ?ASSERT(1.0 =:= row_get(Var, Row)),
  ADT =
    lists:foldl(fun(RK, ADT1) when RK =:= Key -> ADT1;
		   (RK, ADT1) ->
		    R = GetFun(RK, ADT1),
		    PutFun(RK, row_addmul(R, Row, -row_get(Var, R)), ADT1)
		end, ADT0, LookupFun(Var, ADT0)),
  [Key] = LookupFun(Var, ADT),
  ADT.

-spec solve(eq_system(), eq_assoc()) -> error | {ok, solution()}.
solve(EQS0, EqAssoc0) ->
  try triangelise(EQS0, EqAssoc0)
  of {EQS1, EqAssoc} ->
      {ok, solve_1(EqAssoc, maps:from_list(EqAssoc), EQS1, [])}
  catch error -> error
  end.

solve_1([], _VarEqs, _EQS, Acc) -> Acc;
solve_1([{V,K}|Ps], VarEqs, EQS0, Acc0) ->
  Row0 = eqs_get(K, EQS0),
  VarsToKill = [Var || {Var, _} <- row_coefs(Row0), Var =/= V],
  Row1 = kill_vars(VarsToKill, VarEqs, EQS0, Row0),
  [{V,_}] = row_coefs(Row1), % assertion
  Row = row_normalise(V, Row1),
  [{V,1.0}] = row_coefs(Row), % assertion
  EQS = eliminate(V, K, Row, EQS0),
  [K] = eqs_lookup(V, EQS),
  solve_1(Ps, VarEqs, eqs_remove(K, EQS), [{V, row_const(Row)}|Acc0]).

kill_vars([], _VarEqs, _EQS, Row) -> Row;
kill_vars([V|Vs], VarEqs, EQS, Row0) ->
  VRow0 = eqs_get(maps:get(V, VarEqs), EQS),
  VRow = row_normalise(V, VRow0),
  ?ASSERT(1.0 =:= row_get(V, VRow)),
  Row = row_addmul(Row0, VRow, -row_get(V, Row0)),
  ?ASSERT(0.0 =:= row_get(V, Row)), % V has been killed
  kill_vars(Vs, VarEqs, EQS, Row).

-spec weight(label(), bb_weights()) -> float().
weight(Lbl, Weights) ->
  maps:get(Lbl, Weights).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Row datatype
%% Invariant: No 0.0 coefficiets!
-spec row_empty() -> row().
row_empty() -> {orddict:new(), 0.0}.

-spec row_new([{var(), float()}], float()) -> row().
row_new(Coefs, Const) when is_float(Const) ->
  row_ensure_invar({row_squash_multiples(lists:keysort(1, Coefs)), Const}).

row_squash_multiples([{K, C1},{K, C2}|Ps]) ->
  row_squash_multiples([{K,C1+C2}|Ps]);
row_squash_multiples([P|Ps]) -> [P|row_squash_multiples(Ps)];
row_squash_multiples([]) -> [].

row_ensure_invar({Coef, Const}) ->
  {orddict:filter(fun(_, 0.0) -> false; (_, F) when is_float(F) -> true end,
		  Coef), Const}.

row_const({_, Const}) -> Const.
row_coefs({Coefs, _}) -> orddict:to_list(Coefs).
row_size({Coefs, _}) -> orddict:size(Coefs).

row_get(Var, {Coefs, _}) ->
  case lists:keyfind(Var, 1, Coefs) of
    false -> 0.0;
    {_, Coef} -> Coef
  end.

row_set_coef(Var, 0.0, {Coefs, Const}) ->
  {orddict:erase(Var, Coefs), Const};
row_set_coef(Var, Coef, {Coefs, Const}) ->
  {orddict:store(Var, Coef, Coefs), Const}.

row_set_const(Const, {Coefs, _}) -> {Coefs, Const}.

%% Lhs + Rhs*Factor
-spec row_addmul(row(), row(), float()) -> row().
row_addmul({LhsCoefs, LhsConst}, {RhsCoefs, RhsConst}, Factor)
  when is_float(Factor) ->
  Coefs = row_addmul_coefs(LhsCoefs, RhsCoefs, Factor),
  Const = LhsConst + RhsConst * Factor,
  {Coefs, Const}.

row_addmul_coefs(Ls, [], Factor) when is_float(Factor) -> Ls;
row_addmul_coefs([], Rs, Factor) when is_float(Factor) ->
  row_scale_coefs(Rs, Factor);
row_addmul_coefs([L={LV, _}|Ls], Rs=[{RV,_}|_], Factor)
  when LV < RV, is_float(Factor) ->
  [L|row_addmul_coefs(Ls, Rs, Factor)];
row_addmul_coefs(Ls=[{LV, _}|_], [{RV, RC}|Rs], Factor)
  when LV > RV, is_float(RC), is_float(Factor) ->
  [{RV, RC*Factor}|row_addmul_coefs(Ls, Rs, Factor)];
row_addmul_coefs([{V, LC}|Ls], [{V, RC}|Rs], Factor)
  when is_float(LC), is_float(RC), is_float(Factor) ->
  case LC + RC * Factor of
    0.0 ->      row_addmul_coefs(Ls, Rs, Factor);
    C -> [{V,C}|row_addmul_coefs(Ls, Rs, Factor)]
  end.

row_scale(_, 0.0) -> row_empty();
row_scale({RowCoefs, RowConst}, Factor) when is_float(Factor) ->
  {row_scale_coefs(RowCoefs, Factor), RowConst * Factor}.

row_scale_coefs([{V,C}|Cs], Factor) when is_float(Factor), is_float(C) ->
  [{V,C*Factor}|row_scale_coefs(Cs, Factor)];
row_scale_coefs([], Factor) when is_float(Factor) ->
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Equation system ADT
%%
%% Stores a linear equation system, allowing for efficient updates and efficient
%% queries for all equations mentioning a variable.
%%
%% It is sort of like a "database" table of {Primary, Terms, Const} indexed both
%% on Primary as well as the vars (map keys) in Terms.
-type row()       :: {Terms :: orddict:orddict(var(), float()),
		      Const :: float()}.
-type key()       :: non_neg_integer().
-type rev_index() :: #{var() => ordsets:ordset(key())}.
-record(eq_system, {
	  rows = #{}              :: #{key() => row()},
	  revidx = revidx_empty() :: rev_index(),
	  next_key = 0            :: key()
	 }).
-type eq_system() :: #eq_system{}.

eqs_new() -> #eq_system{}.

-spec eqs_insert(row(), eq_system()) -> {key(), eq_system()}.
eqs_insert(Row, EQS=#eq_system{next_key=NextKey0}) ->
  Key = NextKey0,
  NextKey = NextKey0 + 1,
  {Key, eqs_insert(Key, Row, EQS#eq_system{next_key=NextKey})}.

eqs_insert(Key, Row, EQS=#eq_system{rows=Rows, revidx=RevIdx0}) ->
  RevIdx = revidx_add(Key, Row, RevIdx0),
  EQS#eq_system{rows=Rows#{Key => Row}, revidx=RevIdx}.

eqs_put(Key, Row, EQS0) ->
  eqs_insert(Key, Row, eqs_remove(Key, EQS0)).

eqs_remove(Key, EQS=#eq_system{rows=Rows, revidx=RevIdx0}) ->
  OldRow = maps:get(Key, Rows),
  RevIdx = revidx_remove(Key, OldRow, RevIdx0),
  EQS#eq_system{rows = maps:remove(Key, Rows), revidx=RevIdx}.

-spec eqs_get(key(), eq_system()) -> row().
eqs_get(Key, #eq_system{rows=Rows}) -> maps:get(Key, Rows).

%% Keys of all equations containing a nonzero coefficient for Var
-spec eqs_lookup(var(), eq_system()) -> ordsets:ordset(key()).
eqs_lookup(Var, #eq_system{revidx=RevIdx}) -> maps:get(Var, RevIdx).

%% eqs_rows(#eq_system{rows=Rows}) -> maps:to_list(Rows).

%% eqs_print(EQS) ->
%%   lists:foreach(fun({_, Row}) ->
%% 		    row_print(Row)
%% 		end, lists:sort(eqs_rows(EQS))).

%% row_print(Row) ->
%%   CoefStrs = [io_lib:format("~wl~w", [Coef, Var])
%% 	      || {Var, Coef} <- row_coefs(Row)],
%%   CoefStr = lists:join(" + ", CoefStrs),
%%   io:format("~w = ~s~n", [row_const(Row), CoefStr]).

revidx_empty() -> #{}.

-spec revidx_add(key(), row(), rev_index()) -> rev_index().
revidx_add(Key, Row, RevIdx0) ->
  orddict:fold(fun(Var, _Coef, RevIdx1) ->
		?ASSERT(_Coef /= 0.0),
		RevIdx1#{Var => ordsets:add_element(
				  Key, maps:get(Var, RevIdx1, ordsets:new()))}
	    end, RevIdx0, row_coefs(Row)).

-spec revidx_remove(key(), row(), rev_index()) -> rev_index().
revidx_remove(Key, {Coefs, _}, RevIdx0) ->
  orddict:fold(fun(Var, _Coef, RevIdx1) ->
		case RevIdx1 of
		  #{Var := Keys0} ->
		    case ordsets:del_element(Key, Keys0) of
		      [] -> maps:remove(Var, RevIdx1);
		      Keys -> RevIdx1#{Var := Keys}
		    end
		end
	    end, RevIdx0, Coefs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(FAST_ITERATIONS, 5).

%% @doc Computes a rough approximation of BB weights. The approximation is
%% particularly poor (converges slowly) for recursive functions and loops.
-spec compute_fast(cfg(), target_module(), target_context()) -> bb_weights().
compute_fast(CFG, TgtMod, TgtCtx) ->
  Target = {TgtMod, TgtCtx},
  StartLb = hipe_gen_cfg:start_label(CFG),
  RPO = reverse_postorder(CFG, Target),
  PredProbs = [{L, pred_prob(L, CFG, Target)} || L <- RPO, L =/= StartLb],
  Probs0 = (maps:from_list([{L, 0.0} || L <- RPO]))#{StartLb := 1.0},
  fast_iterate(?FAST_ITERATIONS, PredProbs, Probs0).

fast_iterate(0, _Pred, Probs) -> Probs;
fast_iterate(Iters, Pred, Probs0) ->
  fast_iterate(Iters-1, Pred,
	       fast_one(Pred, Probs0)).

fast_one([{L, Pred}|Ls], Probs0) ->
  Weight = fast_sum(Pred, Probs0, 0.0),
  Probs = Probs0#{L => Weight},
  fast_one(Ls, Probs);
fast_one([], Probs) ->
  Probs.

fast_sum([{P,EWt}|Pred], Probs, Acc) when is_float(EWt), is_float(Acc) ->
  case Probs of
    #{P := PWt} when is_float(PWt) ->
      fast_sum(Pred, Probs, Acc + PWt * EWt)
  end;
fast_sum([], _Probs, Acc) when is_float(Acc) ->
  Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Target module interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TGT_IFACE_0(N), N(         {M,C}) -> M:N(         C)).
-define(TGT_IFACE_1(N), N(A1,      {M,C}) -> M:N(A1,      C)).
-define(TGT_IFACE_2(N), N(A1,A2,   {M,C}) -> M:N(A1,A2,   C)).
-define(TGT_IFACE_3(N), N(A1,A2,A3,{M,C}) -> M:N(A1,A2,A3,C)).

?TGT_IFACE_2(bb).
?TGT_IFACE_1(branch_preds).
?TGT_IFACE_1(labels).
?TGT_IFACE_1(reverse_postorder).
