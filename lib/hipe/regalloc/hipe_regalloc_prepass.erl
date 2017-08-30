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
%%	       PREPASS FOR ITERATED REGISTER ALLOCATORS
%%
%% Implements a trivial partial but optimal fast register allocator to be used
%% as the first pass of the register allocation loop.
%%
%% The idea is to drastically reduce the number of temporaries, so as to speed
%% up the real register allocators.
%%
%%  * Spills trivially unallocatable temps
%%    This relies on the fact that calls intentionally clobber all registers.
%%    Since this is the case, any temp that is alive over a call can't possibly
%%    be allocated to anything but a spill slot.
%%
%%  * Partitions the program at points where no pseudos that were not spiled are
%%    live, and then do register allocation on these partitions independently.
%%    These program points are commonly, but not exclusively, the call
%%    instructions.
%%
%% TODO
%%  * This module seems very successful at finding every single spill; register
%%    allocation performance should be improved if we short-circuit the first
%%    hipe_regalloc_loop iteration, skipping directly to rewrite without ever
%%    calling RegAllocMod.
-module(hipe_regalloc_prepass).
-export([regalloc/8, regalloc_initial/8]).

-ifndef(DEBUG).
-compile(inline).
-endif.

%%-define(DO_ASSERT, 1).
-include("../main/hipe.hrl").

%%% TUNABLES

%% Partitions with fewer than ?TUNE_TOO_FEW_BBS basic block halves are merged
%% together before register allocation.
-define(TUNE_TOO_FEW_BBS, 256).

%% Ignore the ra_partitioned option (and do whole function RA instead) when
%% there are fewer than ?TUNE_MIN_SPLIT_BBS basic blocks.
-define(TUNE_MIN_SPLIT_BBS, 384).

%% We present a "pseudo-target" to the register allocator we wrap.
-export([analyze/2,
	 all_precoloured/1,
	 allocatable/1,
	 args/2,
	 bb/3,
	 def_use/2,
	 defines/2,
	 is_fixed/2,	% used by hipe_graph_coloring_regalloc
	 is_global/2,
	 is_move/2,
	 is_precoloured/2,
	 labels/2,
	 livein/3,
	 liveout/3,
	 non_alloc/2,
	 number_of_temporaries/2,
	 physical_name/2,
	 postorder/2,
	 reg_nr/2,
	 uses/2,
	 var_range/2,
	 reverse_postorder/2]).

-record(prepass_ctx,
	{target_mod :: module()
	,target_ctx :: target_context()
	,sub        :: sub_map() % Translates temp numbers found in CFG and understood by
				 % Target to temp numbers passed to RegAllocMod.
	,inv        :: inv_map() % Translates temp numbers passed to RegAllocMod
				 % to temp numbers found in CFG and understood by
				 % Target
	,max_phys   :: temp()    % Exclusive upper bound on physical registers
	}).

-record(cfg,
	{cfg        :: target_cfg()
	,bbs        :: transformed_bbs()
	,max_reg    :: temp()    % Exclusive upper bound on temp numbers
	,rpostorder :: undefined % Only precomputed with partitioned cfg
		     | [label()]
	}).

-type bb()      :: hipe_bb:bb(). % containing instr()
-type liveset() :: ordsets:ordset(temp()).
-record(transformed_bb,
	{bb      :: bb()
	,livein  :: liveset()
	,liveout :: liveset()
	}).
-type transformed_bb() :: #transformed_bb{}.
-type transformed_bbs() :: #{label() => transformed_bb()}.

-record(instr,
	{defuse    :: {[temp()], [temp()]}
	,is_move   :: boolean()
	}).
-type instr() :: #instr{}.

-type target_cfg() :: any().
-type target_instr() :: any().
-type target_temp() :: any().
-type target_reg() :: non_neg_integer().
-type target_liveness() :: any().
-type target_liveset() :: ordsets:ordset(target_reg()).
-type target_context() :: any().
-type spillno() :: non_neg_integer().
-type temp() :: non_neg_integer().
-type label() :: non_neg_integer().

-spec regalloc(module(), target_cfg(), target_liveness(), spillno(), spillno(),
	       module(), target_context(), proplists:proplist())
	      -> {hipe_map(), spillno()}.
regalloc(RegAllocMod, CFG, Liveness, SpillIndex0, SpillLimit, TargetMod,
	 TargetCtx, Options) ->
  {Coloring, SpillIndex, same} =
    regalloc_1(RegAllocMod, CFG, SpillIndex0, SpillLimit, TargetMod,
	       TargetCtx, Options, Liveness),
  {Coloring, SpillIndex}.

%% regalloc_initial/7 is allowed to introduce new temporaries, unlike
%% regalloc/7.
%% In order for regalloc/7 to never introduce temporaries, regalloc/7 must never
%% choose to do split allocation unless regalloc_initial/7 does. This is the
%% reason that the splitting heuristic is solely based on the number of basic
%% blocks, which does not change during the register allocation loop.
-spec regalloc_initial(module(), target_cfg(), target_liveness(), spillno(),
		       spillno(), module(), target_context(),
		       proplists:proplist())
		      -> {hipe_map(), spillno(), target_cfg(),
			  target_liveness()}.
regalloc_initial(RegAllocMod, CFG0, Liveness0, SpillIndex0, SpillLimit,
		 TargetMod, TargetCtx, Options) ->
  {Coloring, SpillIndex, NewCFG} =
    regalloc_1(RegAllocMod, CFG0, SpillIndex0, SpillLimit, TargetMod, TargetCtx,
	       Options, Liveness0),
  {CFG, Liveness} =
    case NewCFG of
      same -> {CFG0, Liveness0};
      {rewritten, CFG1} -> {CFG1, TargetMod:analyze(CFG1, TargetCtx)}
    end,
  {Coloring, SpillIndex, CFG, Liveness}.

regalloc_1(RegAllocMod, CFG0, SpillIndex0, SpillLimit, TargetMod, TargetCtx,
	   Options, Liveness) ->
  {ScanBBs, Seen, SpillMap, SpillIndex1} =
    scan_cfg(CFG0, Liveness, SpillIndex0, TargetMod, TargetCtx),

  {PartColoring, SpillIndex, NewCFG} =
    case proplists:get_bool(ra_partitioned, Options)
      andalso length(TargetMod:labels(CFG0, TargetCtx)) > ?TUNE_MIN_SPLIT_BBS
    of
      true ->
	regalloc_partitioned(SpillMap, SpillIndex1, SpillLimit, ScanBBs,
			     CFG0, TargetMod, TargetCtx, RegAllocMod, Options);
      _ ->
	regalloc_whole(Seen, SpillMap, SpillIndex1, SpillLimit, ScanBBs,
		       CFG0, TargetMod, TargetCtx, RegAllocMod, Options)
    end,

  SpillColors = [{T, {spill, S}} || {T, S} <- maps:to_list(SpillMap)],
  Coloring = SpillColors ++ PartColoring,

  ?ASSERT(begin
	    AllPrecoloured = TargetMod:all_precoloured(TargetCtx),
	    MaxPhys = lists:max(AllPrecoloured) + 1,
	    Unused = unused(live_pseudos(Seen, SpillMap, MaxPhys),
			    SpillMap, CFG0, TargetMod, TargetCtx),
	    unused_unused(Unused, CFG0, TargetMod, TargetCtx)
	  end),
  ?ASSERT(begin
	    CFG =
	      case NewCFG of
		same -> CFG0;
		{rewritten, CFG1} -> CFG1
	      end,
	    check_coloring(Coloring, CFG, TargetMod, TargetCtx)
	  end), % Sanity-check
  ?ASSERT(just_as_good_as(RegAllocMod, CFG, Liveness, SpillIndex0, SpillLimit,
			  TargetMod, TargetCtx, Options, SpillMap, Coloring,
			  Unused)),
  {Coloring, SpillIndex, NewCFG}.

regalloc_whole(Seen, SpillMap, SpillIndex0, SpillLimit, ScanBBs,
	       CFG, TargetMod, TargetCtx, RegAllocMod, Options) ->
  AllPrecoloured = TargetMod:all_precoloured(TargetCtx),
  MaxPhys = lists:max(AllPrecoloured) + 1,
  LivePseudos = live_pseudos(Seen, SpillMap, MaxPhys),
  {SubMap, InvMap, MaxPhys, MaxR, SubSpillLimit} =
    number_and_map(AllPrecoloured, LivePseudos, SpillLimit),
  BBs = transform_whole_cfg(ScanBBs, SubMap),
  SubMod = #cfg{cfg=CFG, bbs=BBs, max_reg=MaxR},
  SubContext = #prepass_ctx{target_mod=TargetMod, target_ctx=TargetCtx,
			    max_phys=MaxPhys, inv=InvMap, sub=SubMap},
  {SubColoring, SpillIndex} =
    RegAllocMod:regalloc(SubMod, SubMod, SpillIndex0, SubSpillLimit, ?MODULE,
			 SubContext, Options),
  ?ASSERT(check_coloring(SubColoring, SubMod, ?MODULE, SubContext)),
  {translate_coloring(SubColoring, InvMap), SpillIndex, same}.

regalloc_partitioned(SpillMap, SpillIndex0, SpillLimit, ScanBBs,
		     CFG, TargetMod, TargetCtx, RegAllocMod, Options) ->
  AllPrecoloured = TargetMod:all_precoloured(TargetCtx),
  MaxPhys = lists:max(AllPrecoloured) + 1,

  DSets0 = initial_dsets(CFG, TargetMod, TargetCtx),
  PartBBList = part_cfg(ScanBBs, SpillMap, MaxPhys),
  DSets1 = join_whole_blocks(PartBBList, DSets0),
  {PartBBsRLList, DSets2} = merge_small_parts(DSets1),
  {PartBBs, DSets3} = merge_pointless_splits(PartBBList, ScanBBs, DSets2),
  SeenMap = collect_seenmap(PartBBsRLList, PartBBs),
  {RPostMap, _DSets4} = part_order(TargetMod:reverse_postorder(CFG, TargetCtx),
				   DSets3),

  {Allocations, SpillIndex} =
    lists:mapfoldl(
      fun({Root, Elems}, SpillIndex1) ->
	  #{Root := Seen} = SeenMap,
	  #{Root := RPost} = RPostMap,
	  LivePseudos = live_pseudos(Seen, SpillMap, MaxPhys),
	  {SubMap, InvMap, MaxPhys, MaxR, SubSpillLimit} =
	    number_and_map(AllPrecoloured, LivePseudos, SpillLimit),
	  BBs = transform_cfg(Elems, PartBBs, SubMap),
	  SubMod = #cfg{cfg=CFG, bbs=BBs, max_reg=MaxR, rpostorder=RPost},
	  SubContext = #prepass_ctx{target_mod=TargetMod, target_ctx=TargetCtx,
				    max_phys=MaxPhys, inv=InvMap, sub=SubMap},
	  {SubColoring, SpillIndex2} =
	    RegAllocMod:regalloc(SubMod, SubMod, SpillIndex1, SubSpillLimit,
				 ?MODULE, SubContext, Options),
	  ?ASSERT(check_coloring(SubColoring, SubMod, ?MODULE, SubContext)),
	  {{translate_coloring(SubColoring, InvMap), Elems}, SpillIndex2}
      end, SpillIndex0, PartBBsRLList),
  {Coloring, NewCFG} =
    combine_allocations(Allocations, MaxPhys, PartBBs, TargetMod, TargetCtx,
			CFG),
  {Coloring, SpillIndex, NewCFG}.

-spec number_and_map([target_reg()], target_liveset(), target_reg())
		    -> {sub_map(), inv_map(), temp(), temp(), temp()}.
number_and_map(Phys, Pseud, SpillLimit) ->
  MaxPhys = lists:max(Phys) + 1,
  ?ASSERT(Pseud =:= [] orelse lists:min(Pseud) >= MaxPhys),
  NrPseuds = length(Pseud),
  MaxR = MaxPhys+NrPseuds,
  PseudNrs = lists:zip(Pseud, lists:seq(MaxPhys, MaxR-1)),
  MapList = lists:zip(Phys, Phys) % Physicals are identity-mapped
    ++ PseudNrs,
  ?ASSERT(MapList =:= lists:ukeysort(1, MapList)),
  SubMap = {s,maps:from_list(MapList)},
  InvMap = {i,maps:from_list([{Fake, Real} || {Real, Fake} <- MapList])},
  SubSpillLimit = translate_spill_limit(MapList, SpillLimit),
  {SubMap, InvMap, MaxPhys, MaxR, SubSpillLimit}.

-spec translate_spill_limit([{target_reg(), temp()}], target_reg()) -> temp().
translate_spill_limit([{Real,Fake}], SpillLimit) when Real < SpillLimit ->
  Fake + 1;
translate_spill_limit([{Real,_}|Ps], SpillLimit) when Real < SpillLimit ->
  translate_spill_limit(Ps, SpillLimit);
translate_spill_limit([{Real,Fake}|_], SpillLimit) when Real >= SpillLimit ->
  Fake.

-spec live_pseudos(seen(), spill_map(), target_reg()) -> target_liveset().
live_pseudos(Seen, SpillMap, MaxPhys) ->
  %% When SpillMap is much larger than Seen (which is typical in the partitioned
  %% case), it is much more efficient doing it like this than making an ordset
  %% of the spills and subtracting.
  ordsets:from_list(
    lists:filter(fun(R) -> R >= MaxPhys andalso not maps:is_key(R, SpillMap)
		 end, maps:keys(Seen))).

-spec translate_coloring(hipe_map(), inv_map()) -> hipe_map().
translate_coloring(SubColoring, InvMap) ->
  lists:map(fun({T, P}) -> {imap_get(T, InvMap), P} end, SubColoring).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% First pass
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Spill trivially unallocatable temps, create internal target-independent
%% program representation, and collect a set of all used temps.
-record(spill_state,
	{map :: spill_map()
	,ix  :: spillno()
	}).
-type spill_state() :: #spill_state{}.
-type spill_map()   :: #{target_reg() => spillno()}.

-spec scan_cfg(target_cfg(), target_liveness(), spillno(), module(),
	       target_context())
	      -> {scan_bbs()
		 ,seen()
		 ,spill_map()
		 ,spillno()
		 }.
scan_cfg(CFG, Liveness, SpillIndex0, TgtMod, TgtCtx) ->
  State0 = #spill_state{map=#{}, ix=SpillIndex0},
  {BBs, Seen, #spill_state{map=Spill, ix=SpillIndex}} =
    scan_bbs(TgtMod:labels(CFG,TgtCtx), CFG, Liveness, #{}, State0, #{}, TgtMod,
	     TgtCtx),
  {BBs, Seen, Spill, SpillIndex}.

-type seen() :: #{target_reg() => []}. % set
-type scan_bb() :: {[instr()], target_liveset(), target_liveset()}.
-type scan_bbs() :: #{label() => scan_bb()}.

-spec scan_bbs([label()], target_cfg(), target_liveness(), seen(),
	       spill_state(), scan_bbs(), module(), target_context())
	      -> {scan_bbs(), seen(), spill_state()}.
scan_bbs([], _CFG, _Liveness, Seen, State, BBs, _TgtMod, _TgtCtx) ->
  {BBs, Seen, State};
scan_bbs([L|Ls], CFG, Liveness, Seen0, State0, BBs, TgtMod, TgtCtx) ->
  Liveout = t_liveout(Liveness, L, TgtMod, TgtCtx),
  {Code, Livein, Seen, State} =
    scan_bb(lists:reverse(hipe_bb:code(TgtMod:bb(CFG, L, TgtCtx))), Liveout,
	    Seen0, State0, [], TgtMod, TgtCtx),
  BB = {Code, Livein, Liveout},
  scan_bbs(Ls, CFG, Liveness, Seen, State, BBs#{L=>BB}, TgtMod, TgtCtx).

-spec scan_bb([target_instr()], target_liveset(), seen(), spill_state(),
	      [instr()], module(), target_context())
	     -> {[instr()]
		,target_liveset()
		,seen()
		,spill_state()
		}.
scan_bb([], Live, Seen, State, IAcc, _TgtMod, _TgtCtx) ->
  {IAcc, Live, Seen, State};
scan_bb([I|Is], Live0, Seen0, State0, IAcc0, TgtMod, TgtCtx) ->
  {TDef, TUse} = TgtMod:def_use(I,TgtCtx),
  ?ASSERT(TDef =:= TgtMod:defines(I,TgtCtx)),
  ?ASSERT(TUse =:= TgtMod:uses(I,TgtCtx)),
  Def = ordsets:from_list(reg_names(TDef, TgtMod, TgtCtx)),
  Use = ordsets:from_list(reg_names(TUse, TgtMod, TgtCtx)),
  Live = ordsets:union(Use, ToSpill = ordsets:subtract(Live0, Def)),
  Seen = add_seen(Def, add_seen(Use, Seen0)),
  NewI = #instr{defuse={Def, Use}, is_move=TgtMod:is_move(I,TgtCtx)},
  IAcc = [NewI|IAcc0],
  State =
    case TgtMod:defines_all_alloc(I,TgtCtx) of
      false -> State0;
      true -> spill_all(ToSpill, TgtMod, TgtCtx, State0)
    end,
  %% We can drop "no-ops" here; where (if anywhere) is it worth it?
  scan_bb(Is, Live, Seen, State, IAcc, TgtMod, TgtCtx).

-spec t_liveout(target_liveness(), label(), module(), target_context()) ->
		   target_liveset().
t_liveout(Liveness, L, TgtMod, TgtCtx) ->
  %% FIXME: unnecessary sort; liveout is sorted, reg_names(...) should be sorted
  %% or consist of a few sorted subsequences (per type)
  ordsets:from_list(reg_names(TgtMod:liveout(Liveness, L, TgtCtx), TgtMod,
			      TgtCtx)).

-spec reg_names([target_temp()], module(), target_context()) -> [target_reg()].
reg_names(Regs, TgtMod, TgtCtx) ->
  [TgtMod:reg_nr(X,TgtCtx) || X <- Regs].

-spec add_seen([target_reg()], seen()) -> seen().
add_seen([], Seen) -> Seen;
add_seen([R|Rs], Seen) -> add_seen(Rs, Seen#{R=>[]}).

-spec spill_all([target_reg()], module(), target_context(), spill_state()) ->
		   spill_state().
spill_all([], _TgtMod, _TgtCtx, State) -> State;
spill_all([R|Rs], TgtMod, TgtCtx, State=#spill_state{map=Map, ix=Ix}) ->
  case TgtMod:is_precoloured(R,TgtCtx) or maps:is_key(R, Map) of
    true -> spill_all(Rs, TgtMod, TgtCtx, State);
    false -> spill_all(Rs, TgtMod, TgtCtx,
		       State#spill_state{map=Map#{R=>Ix}, ix=Ix+1})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Second pass (without split)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rewrite CFG to the new temp names.
-spec transform_whole_cfg(scan_bbs(), sub_map()) -> transformed_bbs().
transform_whole_cfg(BBs0, SubMap) ->
  maps:map(fun(_, BB) -> transform_whole_bb(BB, SubMap) end, BBs0).

-spec transform_whole_bb(scan_bb(), sub_map()) -> transformed_bb().
transform_whole_bb({Code, Livein, Liveout}, SubMap) ->
  #transformed_bb{
     bb=hipe_bb:mk_bb([I#instr{defuse={smap_get_all_partial(Def, SubMap),
				       smap_get_all_partial(Use, SubMap)}}
		       || I = #instr{defuse={Def,Use}} <- Code])
     %% Assume mapping preserves monotonicity
    ,livein=smap_get_all_partial(Livein, SubMap)
    ,liveout=smap_get_all_partial(Liveout, SubMap)
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Second pass (with split)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Discover program partitioning
%% Regretfully, this needs to be a separate pass, as having the global live set
%% is crucial to get a useful partitioning.

%% Single-block parts are merged if there are multiple in a single block, as it
%% is judged to not be beneficial to make them too small.

-type part_bb_part() :: {[instr()], target_liveset(), target_liveset()}.
-type part_bb()  :: {single, part_bb_part()}
		  | {split, part_bb_part(), part_bb_part()}.
-type part_bb_list() :: [{label(), part_bb()}].
-type part_bbs() :: #{label() => part_bb()}.
-type part_bb_sofar() :: single
		       | {split, [instr()], target_liveset()}. % , target_liveset()

-spec part_cfg(scan_bbs(), spill_map(), target_reg()) -> part_bb_list().
part_cfg(ScanBBs, SpillMap, MaxPhys) ->
  Liveset = mk_part_liveset(SpillMap, MaxPhys),
  lists:map(fun(BB) -> part_bb(BB, Liveset) end, maps:to_list(ScanBBs)).

-spec part_bb({label(), scan_bb()}, part_liveset()) -> {label(), part_bb()}.
part_bb({L, BB0={Code0, Livein, Liveout}}, Liveset) ->
  {Sofar, NewCode} = part_bb_1(lists:reverse(Code0), Liveset, Liveout, []),
  BB = case Sofar of
	 single ->
	   ?ASSERT(Code0 =:= NewCode),
	   {single, BB0};
	 {split, ExitCode, ExitLivein = EntryLiveout} ->
	   {split, {NewCode, Livein, EntryLiveout},
	    {ExitCode, ExitLivein, Liveout}}
       end,
  {L, BB}.

-spec part_bb_1([instr()], part_liveset(), target_liveset(), [instr()])
	     -> {part_bb_sofar(), [instr()]}.
part_bb_1([], _Liveset, _Livein, IAcc) -> {single, IAcc};
part_bb_1([I=#instr{defuse={Def,Use}}|Is], Liveset, Live0, IAcc0) ->
  Live = ordsets:union(Use, ordsets:subtract(Live0, Def)),
  IAcc = [I|IAcc0],
  case part_none_live(Live, Liveset) of
    false -> part_bb_1(Is, Liveset, Live, IAcc);
    %% One split point will suffice
    true -> {{split, IAcc, Live}, lists:reverse(Is)}
  end.

-spec part_none_live(target_liveset(), part_liveset()) -> boolean().
part_none_live(Live, Liveset) ->
  not lists:any(fun(R) -> part_liveset_is_live(R, Liveset) end, Live).

-type part_liveset() :: {spill_map(), target_reg()}.

-spec mk_part_liveset(spill_map(), target_reg()) -> part_liveset().
mk_part_liveset(SpillMap, MaxPhys) -> {SpillMap, MaxPhys}.

-spec part_liveset_is_live(target_reg(), part_liveset()) -> boolean().
part_liveset_is_live(R, {SpillMap, MaxPhys}) when is_integer(R) ->
  R >= MaxPhys andalso not maps:is_key(R, SpillMap).

%% @doc Merges split blocks where entry and exit belong to the same DSet.
%% Does not change DSets
-spec merge_pointless_splits(part_bb_list(), scan_bbs(), bb_dsets())
			   -> {part_bbs(), bb_dsets()}.
merge_pointless_splits(PartBBList0, ScanBBs, DSets0) ->
  {PartBBList, DSets} =
    merge_pointless_splits_1(PartBBList0, ScanBBs, DSets0, []),
  {maps:from_list(PartBBList), DSets}.

-spec merge_pointless_splits_1(
	part_bb_list(), scan_bbs(), bb_dsets(), part_bb_list())
			      -> {part_bb_list(), bb_dsets()}.
merge_pointless_splits_1([], _ScanBBs, DSets, Acc) -> {Acc, DSets};
merge_pointless_splits_1([P={_,{single,_}}|Ps], ScanBBs, DSets, Acc) ->
  merge_pointless_splits_1(Ps, ScanBBs, DSets, [P|Acc]);
merge_pointless_splits_1([P0={L,{split,_,_}}|Ps], ScanBBs, DSets0, Acc) ->
  {EntryRoot, DSets1} = hipe_dsets:find({entry,L}, DSets0),
  {ExitRoot,  DSets}  = hipe_dsets:find({exit,L},  DSets1),
  case EntryRoot =:= ExitRoot of
    false -> merge_pointless_splits_1(Ps, ScanBBs, DSets, [P0|Acc]);
    true ->
      %% Reuse the code list from ScanBBs rather than concatenating the split
      %% parts
      #{L := BB} = ScanBBs,
      ?ASSERT(begin
		{L,{split,{_EntryCode,_,_},{_ExitCode,_,_}}}=P0, % [_|
		{_Code,_,_}=BB,
		_Code =:= (_EntryCode ++ _ExitCode)
	      end),
      merge_pointless_splits_1(Ps, ScanBBs, DSets, [{L,{single, BB}}|Acc])
  end.

-spec merge_small_parts(bb_dsets()) -> {bb_dsets_rllist(), bb_dsets()}.
merge_small_parts(DSets0) ->
  {RLList, DSets1} = hipe_dsets:to_rllist(DSets0),
  RLLList = [{R, length(Elems), Elems} || {R, Elems} <- RLList],
  merge_small_parts_1(RLLList, DSets1, []).

-spec merge_small_parts_1(
	[{bb_dset_key(), non_neg_integer(), [bb_dset_key()]}],
	bb_dsets(), bb_dsets_rllist()
       ) -> {bb_dsets_rllist(), bb_dsets()}.
merge_small_parts_1([], DSets, Acc) -> {Acc, DSets};
merge_small_parts_1([{R, _, Es}], DSets, Acc) -> {[{R, Es}|Acc], DSets};
merge_small_parts_1([{R, L, Es}|Ps], DSets, Acc) when L >= ?TUNE_TOO_FEW_BBS ->
  merge_small_parts_1(Ps, DSets, [{R,Es}|Acc]);
merge_small_parts_1([Fst,{R, L, Es}|Ps], DSets, Acc)
  when L >= ?TUNE_TOO_FEW_BBS ->
  merge_small_parts_1([Fst|Ps], DSets, [{R,Es}|Acc]);
merge_small_parts_1([{R1,L1,Es1},{R2,L2,Es2}|Ps], DSets0, Acc) ->
  ?ASSERT(L1 < ?TUNE_TOO_FEW_BBS andalso L2 < ?TUNE_TOO_FEW_BBS),
  DSets1 = hipe_dsets:union(R1, R2, DSets0),
  {R, DSets} = hipe_dsets:find(R1, DSets1),
  merge_small_parts_1([{R,L2+L1,Es2++Es1}|Ps], DSets, Acc).

%% @doc Partition an ordering over BBs into subsequences for the dsets that
%% contain them.
%% Does not change dsets.
-spec part_order([label()], bb_dsets())
		-> {#{bb_dset_key() => [label()]}, bb_dsets()}.
part_order(Lbs, DSets) -> part_order(Lbs, DSets, #{}).

part_order([], DSets, Acc) -> {Acc, DSets};
part_order([L|Ls], DSets0, Acc0) ->
  {EntryRoot, DSets1} = hipe_dsets:find({entry,L}, DSets0),
  {ExitRoot,  DSets2} = hipe_dsets:find({exit,L},  DSets1),
  Acc1 = map_append(EntryRoot, L, Acc0),
  %% Only include the label once if both entry and exit is in same partition
  Acc2 = case EntryRoot =:= ExitRoot of
	   true -> Acc1;
	   false -> map_append(ExitRoot, L, Acc1)
	 end,
  part_order(Ls, DSets2, Acc2).

map_append(Key, Elem, Map) ->
  case Map of
    #{Key := List} -> Map#{Key := [Elem|List]};
    #{} -> Map#{Key => [Elem]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interference graph partitioning
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We partition the program

%% The algorithm considers two kinds of components; those that are local to a
%% basic block, and those that are not. The key is that any basic block belongs
%% to at most two non-local components; one from the beginning to the first
%% split point, and one from the end to the last split point.

-type bb_dset_key() :: {entry | exit, label()}.
-type bb_dsets() :: hipe_dsets:dsets(bb_dset_key()).
-type bb_dsets_rllist() :: [{bb_dset_key(), [bb_dset_key()]}].

-spec initial_dsets(target_cfg(), module(), target_context()) -> bb_dsets().
initial_dsets(CFG, TgtMod, TgtCtx) ->
  Labels = TgtMod:labels(CFG, TgtCtx),
  DSets0 = hipe_dsets:new(lists:append([[{entry,L},{exit,L}] || L <- Labels])),
  Edges = lists:append([[{L, S} || S <- hipe_gen_cfg:succ(CFG, L)]
			|| L <- Labels]),
  lists:foldl(fun({X, Y}, DS) -> hipe_dsets:union({exit,X}, {entry,Y}, DS) end,
	      DSets0, Edges).

-spec join_whole_blocks(part_bb_list(), bb_dsets()) -> bb_dsets().
join_whole_blocks(PartBBList, DSets0) ->
  lists:foldl(fun({L, {single, _}}, DS) ->
		  hipe_dsets:union({entry,L}, {exit,L}, DS);
		 ({_, {split, _, _}}, DS) -> DS
	      end, DSets0, PartBBList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Third pass
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Collect all referenced temps in each partition.

%% Note: The temps could be collected during the partition pass for each
%% half-bb, and then combined here. Would that be beneficial?

collect_seenmap(PartBBsRLList, PartBBs) ->
  collect_seenmap(PartBBsRLList, #{}, PartBBs).

collect_seenmap([], Acc, _PartBBs) -> Acc;
collect_seenmap([{R,Elems}|Ps], Acc, PartBBs) ->
  Seen = collect_seen_part(Elems, #{}, PartBBs),
  collect_seenmap(Ps, Acc#{R => Seen}, PartBBs).

collect_seen_part([], Acc, _PartBBs) -> Acc;
collect_seen_part([{Half,L}|Es], Acc0, PartBBs) ->
  BB = maps:get(L, PartBBs),
  Code = case {Half, BB} of
	   {entry, {single, {C,_,_}}} -> C;
	   {entry, {split, {C,_,_}, _}} -> C;
	   {exit,  {split, _, {C,_,_}}} -> C;
	   {exit,  {single, _}} -> [] % Ignore; was collected by its entry half
	 end,
  Acc = collect_seen_code(Code, Acc0),
  collect_seen_part(Es, Acc, PartBBs).

collect_seen_code([], Acc) -> Acc;
collect_seen_code([#instr{defuse={Def,Use}}|Is], Acc) ->
  collect_seen_code(Is, add_seen(Def, add_seen(Use, Acc))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fourth pass
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rewrite CFG to the new temp names.
-spec transform_cfg([bb_dset_key()], part_bbs(), sub_map()) -> transformed_bbs().

transform_cfg(Elems, PartBBs, SubMap) ->
  transform_cfg(Elems, PartBBs, SubMap, #{}).

transform_cfg([], _PartBBs, _SubMap, Acc) -> Acc;
transform_cfg([{Half,L}|Es], PartBBs, SubMap, Acc0) ->
  #{L := PBB} = PartBBs,
  Acc = case {Half, PBB} of
	  {entry, {single,BB}}  -> Acc0#{L=>transform_bb(BB, SubMap)};
	  {entry, {split,BB,_}} -> Acc0#{L=>transform_bb(BB, SubMap)};
	  {exit,  {split,_,BB}} -> Acc0#{L=>transform_bb(BB, SubMap)};
	  {exit,  {single, _}}  -> Acc0 % Was included by the entry half
	end,
  transform_cfg(Es, PartBBs, SubMap, Acc).

-spec transform_bb(part_bb_part(), sub_map()) -> transformed_bb().
transform_bb(BB, SubMap) ->
  %% For now, part_bb_part() and split_bb() share representation
  transform_whole_bb(BB, SubMap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fifth pass
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Combine colorings and substitute temps in actual cfg if there were
%% collisions.

%% A temp can sometimes appear in more than one partition. For example, defining
%% an unused value. If these are found by combine_allocations, we have to
%% rename this temp in one of the partitions on the real cfg.
%%
%% We optimistically assume that there will be no such collisions, and when
%% there are, we fix them up as they're found.

-spec combine_allocations([{hipe_map(), [bb_dset_key()]}], target_reg(),
			  part_bbs(), module(), target_context(), target_cfg())
			 -> {hipe_map(), same | {rewritten, target_cfg()}}.
combine_allocations([{A,_}|As], MaxPhys, PartBBs, TgtMod, TgtCtx, CFG) ->
  {Phys, Pseuds} = lists:partition(fun({R,_}) -> R < MaxPhys end, A),
  {Seen, _, []} = partition_by_seen(Pseuds, #{}, [], []),
  combine_allocations(As, MaxPhys, PartBBs, TgtMod, TgtCtx, Phys, Seen, Pseuds,
		      {same, CFG}).

-spec combine_allocations([{hipe_map(), [bb_dset_key()]}], target_reg(),
			  part_bbs(), module(), target_context(), hipe_map(),
			  seen(), hipe_map(), {same|rewritten, target_cfg()})
			 -> {hipe_map(), same | {rewritten, target_cfg()}}.
combine_allocations([], _MaxPhys, _PartBBs, _TgtMod, _TgtCtx, Phys, _Seen,
		    Pseuds, CFGT) ->
  {Phys ++ Pseuds, case CFGT of
		     {same, _} -> same;
		     {rewritten, _} -> CFGT
		   end};
combine_allocations([{A,PartElems}|As], MaxPhys, PartBBs, TgtMod, TgtCtx, Phys,
		    Seen0, Acc, CFGT={_,CFG0}) ->
  {Phys, Pseuds0} = lists:partition(fun({R,_}) -> R < MaxPhys end, A),
  {Seen, Pseuds, Collisions} = partition_by_seen(Pseuds0, Seen0, [], []),
  case Collisions of
    [] -> combine_allocations(As, MaxPhys, PartBBs, TgtMod, TgtCtx, Phys, Seen,
			      Pseuds++Acc, CFGT);
    _ ->
      %% There were collisions; rename all the temp numbers in Collisions
      {CFG, Renamed} = rename(Collisions, PartElems, PartBBs, TgtMod, TgtCtx,
			      CFG0),
      combine_allocations(As, MaxPhys, PartBBs, TgtMod, TgtCtx, Phys, Seen,
			  Pseuds++Renamed++Acc, {rewritten,CFG})
  end.

%% @doc Partitions a coloring on whether the registers are in the Seen set,
%% adding any new registers to the set.
-spec partition_by_seen(hipe_map(), seen(), hipe_map(), hipe_map())
		       -> {seen(), hipe_map(), hipe_map()}.
partition_by_seen([], Seen, Acc, Collisions) -> {Seen, Acc, Collisions};
partition_by_seen([C={R,_}|Cs], Seen, Acc, Colls) ->
  case Seen of
    #{R := _} -> partition_by_seen(Cs, Seen, Acc, [C|Colls]);
    #{}       -> partition_by_seen(Cs, Seen#{R => []}, [C|Acc], Colls)
  end.

-spec rename(hipe_map(), [bb_dset_key()], part_bbs(), module(),
	     target_context(), target_cfg())
	    -> {target_cfg(), hipe_map()}.
rename(CollisionList, PartElems, PartBBs, TgtMod, TgtCtx, CFG0) ->
  {Map, Renamed} = new_names(CollisionList, TgtMod, TgtCtx, #{}, []),
  Fun = fun(I) ->
	    TgtMod:subst_temps(
	      fun(Temp) ->
		  N = TgtMod:reg_nr(Temp, TgtCtx),
		  case Map of
		    #{N := Subst} -> TgtMod:update_reg_nr(Subst, Temp, TgtCtx);
		    #{} -> Temp
		  end
	      end, I, TgtCtx)
	end,
  {rename_1(PartElems, PartBBs, TgtMod, TgtCtx, Fun, CFG0), Renamed}.

-type rename_map() :: #{target_reg() => target_reg()}.
-type rename_fun() :: fun((target_instr()) -> target_instr()).

-spec new_names(hipe_map(), module(), target_context(), rename_map(),
		hipe_map())
	       -> {rename_map(), hipe_map()}.
new_names([], _TgtMod, _TgtCtx, Map, Renamed) -> {Map, Renamed};
new_names([{R,C}|As], TgtMod, TgtCtx, Map, Renamed) ->
  Subst = TgtMod:new_reg_nr(TgtCtx),
  new_names(As, TgtMod, TgtCtx, Map#{R => Subst}, [{Subst, C} | Renamed]).

%% @doc Maps over all instructions in a partition on the original CFG.
-spec rename_1([bb_dset_key()], part_bbs(), module(), target_context(),
	       rename_fun(), target_cfg()) -> target_cfg().
rename_1([], _PartBBs, _TgtMod, _TgtCtx, _Fun, CFG) -> CFG;
rename_1([{Half,L}|Es], PartBBs, TgtMod, TgtCtx, Fun, CFG0) ->
  Code0 = hipe_bb:code(BB = TgtMod:bb(CFG0, L, TgtCtx)),
  Code = case {Half, maps:get(L, PartBBs)} of
	  {entry, {single,_}} -> lists:map(Fun, Code0);
	  {entry, {split,PBBP,_}} ->
	     map_start(Fun, part_bb_part_len(PBBP), Code0);
	  {exit, {split,_,PBBP}} ->
	     map_end(Fun, part_bb_part_len(PBBP), Code0);
	  {exit, {single, _}} -> Code0
	end,
  CFG = TgtMod:update_bb(CFG0, L, hipe_bb:code_update(BB, Code), TgtCtx),
  rename_1(Es, PartBBs, TgtMod, TgtCtx, Fun, CFG).

-spec part_bb_part_len(part_bb_part()) -> non_neg_integer().
part_bb_part_len({Code, _Livein, _Liveout}) -> length(Code).

%% @doc Map the first N elements of a list
-spec map_start(fun((X) -> Y), non_neg_integer(), [X]) -> [X|Y].
map_start(_Fun, 0, List) -> List;
map_start(Fun, N, [E|Es]) ->
  [Fun(E)|map_start(Fun, N-1, Es)].

%% @doc Map the last N elements of a list
-spec map_end(fun((X) -> Y), non_neg_integer(), [X]) -> [X|Y].
map_end(Fun, N, List) ->
  map_end(Fun, N, length(List), List).

map_end(Fun, N, Len, [E|Es]) when Len > N -> [E|map_end(Fun, N, Len-1, Es)];
map_end(Fun, N, Len, List) when Len =:= N -> lists:map(Fun, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Temp map ADT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type sub_map() :: {s,#{target_reg() => temp()}}.
-type inv_map() :: {i,#{temp() => target_reg()}}.

-spec smap_get(target_reg(), sub_map()) -> temp().
smap_get(Temp, {s,Map}) when is_integer(Temp) -> maps:get(Temp, Map).

-spec imap_get(temp(), inv_map()) -> target_reg().
imap_get(Temp, {i,Map}) when is_integer(Temp) -> maps:get(Temp, Map).

-spec smap_get_all_partial([target_reg()], sub_map()) -> [temp()].
smap_get_all_partial([], _) -> [];
smap_get_all_partial([T|Ts], SMap={s,Map}) when is_integer(T) ->
  case Map of
    #{T := R} -> [R|smap_get_all_partial(Ts, SMap)];
    #{} -> smap_get_all_partial(Ts, SMap)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(DO_ASSERT).
%%%%%%%%%%%%%%%%%%%%
%% Check that the coloring is correct (if the IG is correct):
%%

%% Define these as 'ok' or 'report(X,Y)' depending on how much output you want.
-define(report0(X,Y), ?IF_DEBUG_LEVEL(0,?msg(X, Y),ok)).
-define(report(X,Y),  ?IF_DEBUG_LEVEL(1,?msg(X, Y),ok)). 
-define(report2(X,Y), ?IF_DEBUG_LEVEL(2,?msg(X, Y),ok)). 
-define(report3(X,Y), ?IF_DEBUG_LEVEL(3,?msg(X, Y),ok)).

check_coloring(Coloring, CFG, TgtMod, TgtCtx) ->
  ?report0("checking coloring ~p~n",[Coloring]),
  IG = hipe_ig:build(CFG, TgtMod:analyze(CFG,TgtCtx), TgtMod, TgtCtx),
  check_cols(hipe_vectors:list(hipe_ig:adj_list(IG)),
	     init_coloring(Coloring, TgtMod, TgtCtx)).

init_coloring(Xs, TgtMod, TgtCtx) ->
  hipe_temp_map:cols2tuple(Xs, TgtMod, TgtCtx).

check_color_of(X, Cols) ->
  case hipe_temp_map:find(X, Cols) of
    unknown ->
      uncolored;
    C ->
      C
  end.

check_cols([], _Cols) ->
  ?report("coloring valid~n",[]),
  true;
check_cols([{X,Neighbours}|Xs], Cols) ->
  Cs = [{N, check_color_of(N, Cols)} || N <- Neighbours],
  C = check_color_of(X, Cols),
  case valid_coloring(X, C, Cs) of
    yes ->
      check_cols(Xs, Cols);
    {no,Invalids} ->
      ?msg("node ~p has same color (~p) as ~p~n", [X,C,Invalids]),
      check_cols(Xs, Cols) andalso false
  end.

valid_coloring(_X, _C, []) ->
  yes;
valid_coloring(X, C, [{Y,C}|Ys]) ->
  case valid_coloring(X, C, Ys) of
    yes -> {no, [Y]};
    {no,Zs} -> {no, [Y|Zs]}
  end;
valid_coloring(X, C, [_|Ys]) ->
  valid_coloring(X, C, Ys).

unused_unused(Unused, CFG, TgtMod, TgtCtx) ->
  IG = hipe_ig:build(CFG, TgtMod:analyze(CFG,TgtCtx), TgtMod, TgtCtx),
  lists:all(fun(R) -> case hipe_ig:get_node_degree(R, IG) of
			0 -> true;
			Deg ->
			  ?msg("Temp ~w is in unused but has degree ~w~n",
			       [R, Deg]),
			  false
		      end end, Unused).

%%%%%%%%%%%%%%%%%%%%
%% Check that no register allocation opportunities were missed due to ?MODULE
%%
just_as_good_as(RegAllocMod, CFG, Liveness, SpillIndex0, SpillLimit, TgtMod,
		TgtCtx, Options, SpillMap, Coloring, Unused) ->
  {CheckColoring, _} =
    RegAllocMod:regalloc(CFG, Liveness, SpillIndex0, SpillLimit, TgtMod, TgtCtx,
			 Options),
  Now   = lists:sort([{R,Kind} || {R,{Kind,_}} <- Coloring,
				  not ordsets:is_element(R, Unused)]),
  Check = lists:sort([{R,Kind} || {R,{Kind,_}} <- CheckColoring,
				  not ordsets:is_element(R, Unused)]),
  CheckMap = maps:from_list(Check),
  SaneSpills = all_spills_sane_1(CheckColoring, SpillMap),
  case SaneSpills
    andalso lists:all(fun({R, spill}) -> maps:get(R, CheckMap) =:= spill;
			 ({_,reg}) -> true
		      end, Now)
  of
    true -> true;
    false ->
      {NowRegs, _} = _NowCount = count(Now),
      {CheckRegs, _} = _CheckCount = count(Check),
      {M,F,A} = element(2, element(3, CFG)),
      io:fwrite(standard_error, "Colorings differ (~w, ~w)!~n"
		"MFA: ~w:~w/~w~n"
		"Unused: ~w~n"
		"Now:~w~nCorrect:~w~n",
		[TgtMod, RegAllocMod,
		 M,F,A,
		 Unused,
		 Now -- Check, Check -- Now]),
	SaneSpills andalso NowRegs >= CheckRegs
  end.

count(C) -> {length([[] || {_, reg} <- C]),
	     length([[] || {_, spill} <- C])}.

unused(LivePseudos, SpillMap, CFG, TgtMod, TgtCtx) ->
  {TMin, TMax} = TgtMod:var_range(CFG,TgtCtx),
  SpillOSet = ordsets:from_list(maps:keys(SpillMap)),
  PhysOSet = ordsets:from_list(TgtMod:all_precoloured(TgtCtx)),
  Used = ordsets:union(LivePseudos, ordsets:union(PhysOSet, SpillOSet)),
  ordsets:subtract(lists:seq(TMin, TMax), Used).

%% Check that no temp that we wrote off was actually allocatable.
all_spills_sane_1(_, Empty) when map_size(Empty) =:= 0 -> true;
all_spills_sane_1([], _Nonempty) -> false;
all_spills_sane_1([{T, {reg, _}}|Cs], SpillMap) ->
  not maps:is_key(T, SpillMap) andalso all_spills_sane_1(Cs, SpillMap);
all_spills_sane_1([{T, {spill, _}}|Cs], SpillMap) ->
  all_spills_sane_1(Cs, maps:remove(T, SpillMap)).

-endif. % DO_ASSERT

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pseudo-target interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze(Cfg, _ModRec) -> Cfg.
bb(Cfg=#cfg{bbs=BBs}, Ix, _ModRec) ->
  case BBs of
    #{Ix := #transformed_bb{bb=BB}} -> BB;
    _ -> error(badarg, [Cfg, Ix])
  end.
args(Arity, #prepass_ctx{target_mod=TgtMod, target_ctx=TgtCtx, sub=SubM}) ->
  smap_get(TgtMod:args(Arity,TgtCtx), SubM).
labels(#cfg{bbs=BBs}, _ModRec) -> maps:keys(BBs).
livein(#cfg{bbs=BBs}, Lb, _SubMod) ->
  #{Lb := #transformed_bb{livein=Livein}} = BBs,
  Livein.
liveout(#cfg{bbs=BBs}, Lb, _SubMod) ->
  #{Lb := #transformed_bb{liveout=Liveout}} = BBs,
  Liveout.
uses(I, MR) -> element(2, def_use(I, MR)).
defines(I, MR) -> element(1, def_use(I, MR)).
def_use(#instr{defuse=DefUse}, _ModRec) -> DefUse.
is_move(#instr{is_move=IM}, _ModRec) -> IM.
is_fixed(Reg, #prepass_ctx{target_mod=TgtMod,target_ctx=TgtCtx,inv=InvM}) ->
  TgtMod:is_fixed(imap_get(Reg, InvM),TgtCtx). % XXX: Is this hot?
is_global(Reg, #prepass_ctx{target_mod=TgtMod,target_ctx=TgtCtx,
			    max_phys=MaxPhys}) when Reg < MaxPhys ->
  TgtMod:is_global(Reg,TgtCtx). % assume id-map
is_precoloured(Reg, #prepass_ctx{max_phys=MaxPhys}) -> Reg < MaxPhys.
reg_nr(Reg, _ModRec) -> Reg. % After mapping (naturally)
non_alloc(#cfg{cfg=CFG}, #prepass_ctx{target_mod=TgtMod,target_ctx=TgtCtx,
				  sub=SubM}) ->
  smap_get_all_partial(reg_names(TgtMod:non_alloc(CFG,TgtCtx), TgtMod, TgtCtx),
		       SubM).
number_of_temporaries(#cfg{max_reg=MaxR}, _ModRec) -> MaxR.
allocatable(#prepass_ctx{target_mod=TgtMod, target_ctx=TgtCtx}) ->
  TgtMod:allocatable(TgtCtx). % assume id-map
physical_name(Reg, _ModRec) -> Reg.
all_precoloured(#prepass_ctx{target_mod=TgtMod, target_ctx=TgtCtx}) ->
  TgtMod:all_precoloured(TgtCtx). % dito
var_range(#cfg{cfg=_CFG, max_reg=MaxReg},
	  #prepass_ctx{target_mod=_TgtMod, target_ctx=_TgtCtx}) ->
  ?ASSERT(begin {TgtMin, _} = _TgtMod:var_range(_CFG,_TgtCtx),
		TgtMin =:= 0
	  end),
  {0, MaxReg-1}.

postorder(#cfg{cfg=CFG,rpostorder=undefined},
	  #prepass_ctx{target_mod=TgtMod,target_ctx=TgtCtx}) ->
  TgtMod:postorder(CFG,TgtCtx);
postorder(#cfg{rpostorder=Labels}, _ModRec) when is_list(Labels) ->
  lists:reverse(Labels).

reverse_postorder(#cfg{cfg=CFG,rpostorder=undefined},
		  #prepass_ctx{target_mod=TgtMod,target_ctx=TgtCtx}) ->
  TgtMod:reverse_postorder(CFG,TgtCtx);
reverse_postorder(#cfg{rpostorder=Labels}, _ModRec) when is_list(Labels) ->
  Labels.
