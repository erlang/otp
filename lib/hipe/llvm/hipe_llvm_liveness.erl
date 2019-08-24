-module(hipe_llvm_liveness).

-export([analyze/1]).

%% @doc Find gc roots and explicitly mark when they go out of scope, based
%% on the liveness analyzis performed by the hipe_rtl_liveness:analyze/1.
analyze(RtlCfg) ->
  Liveness = hipe_rtl_liveness:analyze(RtlCfg),
  Roots = find_roots(RtlCfg, Liveness),
  %% erlang:display(Roots),
  NewRtlCfg = mark_dead_roots(RtlCfg, Liveness, Roots),
  {NewRtlCfg, Roots}.

%% @doc Determine which are the GC Roots.Possible roots are all
%% RTL variables (rtl_var). However, since safe points are function calls, we
%% consider as possible GC roots only RTL variables that are live around
%% function calls.
find_roots(Cfg, Liveness) ->
  Labels = hipe_rtl_cfg:postorder(Cfg),
  Roots = find_roots_bb(Labels, Cfg, Liveness, []),
  lists:usort(lists:flatten(Roots)).

find_roots_bb([], _Cfg, _Liveness, RootAcc) ->
  RootAcc;
find_roots_bb([L|Ls], Cfg, Liveness, RootAcc) ->
  Block = hipe_rtl_cfg:bb(Cfg, L),
  BlockCode = hipe_bb:code(Block),
  LiveIn = ordsets:from_list(strip(hipe_rtl_liveness:livein(Liveness, L))),
  LiveOut = ordsets:from_list(strip(hipe_rtl_liveness:liveout(Liveness, L))),
  Roots = do_find_roots_bb(BlockCode, L, LiveOut, LiveIn, []),
  find_roots_bb(Ls, Cfg, Liveness, Roots++RootAcc).

%% For each call inside a BB the GC roots are those RTL variables that
%% are live before and after the call.
%% --> Live Before Call: These are the RTL variables that belong to the
%% LiveIn list or are initialized inside the BB before the call
%% --> Live After Call: These are the RTL variables that belong to the
%% LiveOut list or are used after the call inside the BB (they die
%% inside the BB and so do not belong to the LiveOut list)
do_find_roots_bb([], _Label, _LiveOut, _LiveBefore, RootAcc) ->
  RootAcc;
do_find_roots_bb([I|Is], L, LiveOut, LiveBefore, RootAcc) ->
  case hipe_rtl:is_call(I) of
    true ->
      %% Used inside the BB after the call
      UsedAfterCall_ = strip(lists:flatten([hipe_rtl:uses(V) || V <- Is])),
      UsedAfterCall = ordsets:from_list(UsedAfterCall_),
      LiveAfter = ordsets:union(UsedAfterCall, LiveOut),
      %% The Actual Roots
      Roots = ordsets:intersection(LiveBefore, LiveAfter),
      %% The result of the instruction
      Defines = ordsets:from_list(strip(hipe_rtl:defines(I))),
      LiveBefore1 = ordsets:union(LiveBefore, Defines),
      do_find_roots_bb(Is, L, LiveOut, LiveBefore1, [Roots|RootAcc]);
    false ->
      %% The result of the instruction
      Defines = ordsets:from_list(strip(hipe_rtl:defines(I))),
      LiveBefore1 = ordsets:union(LiveBefore, Defines),
      do_find_roots_bb(Is, L, LiveOut, LiveBefore1, RootAcc)
  end.

%% @doc This function is responsible for marking when GC Roots, which can be
%% only RTL variables go out of scope (dead). This pass is needed for the LLVM
%% back end because the LLVM framework forces us to explicit mark when gc roots
%% are no longer live.
mark_dead_roots(CFG, Liveness, Roots) ->
  Labels = hipe_rtl_cfg:postorder(CFG),
  mark_dead_bb(Labels, CFG, Liveness, Roots).

mark_dead_bb([], Cfg, _Liveness, _Roots) ->
  Cfg;
mark_dead_bb([L|Ls], Cfg, Liveness, Roots)  ->
  Block = hipe_rtl_cfg:bb(Cfg, L),
  BlockCode = hipe_bb:code(Block),
  LiveOut = ordsets:from_list(strip(hipe_rtl_liveness:liveout(Liveness, L))),
  NewBlockCode = do_mark_dead_bb(BlockCode, LiveOut, Roots, []),
  %% Update the CFG
  NewBB = hipe_bb:code_update(Block, NewBlockCode),
  NewCFG = hipe_rtl_cfg:bb_add(Cfg, L, NewBB),
  mark_dead_bb(Ls, NewCFG, Liveness, Roots).

do_mark_dead_bb([], _LiveOut, _Roots, NewBlockCode) ->
  lists:reverse(NewBlockCode);
do_mark_dead_bb([I|Is], LiveOut ,Roots, NewBlockCode) ->
  Uses = ordsets:from_list(strip(hipe_rtl:uses(I))),
  %% GC roots that are used in this instruction
  RootsUsed = ordsets:intersection(Roots, Uses),
  UsedAfter_ = strip(lists:flatten([hipe_rtl:uses(V) || V <- Is])),
  UsedAfter = ordsets:from_list(UsedAfter_),
  %% GC roots that are live after this instruction
  LiveAfter = ordsets:union(LiveOut, UsedAfter),
  %% GC roots that their last use is in this instruction
  DeadRoots = ordsets:subtract(RootsUsed, LiveAfter),
  %% Recreate the RTL variable from the corresponding Index
  OldVars = [hipe_rtl:mk_var(V1) || V1 <- DeadRoots],
  %% Mark the RTL variable as DEAD (last use)
  NewVars = [kill_var(V2) || V2 <- OldVars],
  %% Create a list with the substitution of the old vars with the new
  %% ones which are marked with the dead keyword
  Subtitution = lists:zip(OldVars, NewVars),
  NewI = case Subtitution of
    [] -> I;
    _ -> hipe_rtl:subst_uses_llvm(Subtitution, I)
  end,
  do_mark_dead_bb(Is, LiveOut, Roots, [NewI|NewBlockCode]).

%% Update the liveness of a var,in order to mark that this is the last use.
kill_var(Var) -> hipe_rtl:var_liveness_update(Var, dead).

%% We are only interested for rtl_vars, since only rtl_vars are possible gc
%% roots.
strip(L) -> [Y || {rtl_var, Y, _} <- L].
