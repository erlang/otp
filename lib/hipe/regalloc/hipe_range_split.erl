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
%%	       TEMPORARY LIVE RANGE SPLITTING PASS
%%
%% Live range splitting is useful to allow a register allocator to allocate a
%% temporary to register for a part of its lifetime, even if it cannot be for
%% the entirety. This improves register allocation quality, at the cost of
%% making the allocation problem more time and memory intensive to solve.
%%
%% Optimal allocation can be achieved if all temporaries are split at every
%% program point (between all instructions), but this makes register allocation
%% infeasably slow in practice. Instead, this module uses heuristics to choose
%% which temporaries should have their live ranges split, and at which points.
%%
%% The range splitter only considers temps which are live during a call
%% instruction, since they're known to be spilled. The control-flow graph is
%% partitioned at call instructions and splitting decisions are made separately
%% for each partition. The register copy of a temp (if any) gets a separate name
%% in each partition.
%%
%% There are three different ways the range splitter may choose to split a
%% temporary in a program partition:
%%
%%  * Mode1: Spill the temp before calls, and restore it after them
%%  * Mode2: Spill the temp after definitions, restore it after calls
%%  * Mode3: Spill the temp after definitions, restore it before uses
%%
%% To pick which of these should be used for each temp×partiton pair, the range
%% splitter uses a cost function. The cost is simply the sum of the cost of all
%% expected stack accesses, and the cost for an individual stack access is based
%% on the probability weight of the basic block that it resides in. This biases
%% the range splitter so that it attempts moving stack accesses from a functions
%% hot path to the cold path.
%%
%% The heuristic has a couple of tuning knobs, adjusting its preference for
%% different spilling modes, aggressiveness, and how much influence the basic
%% block probability weights have.
%%
%% Edge case not handled: Call instructions directly defining a pseudo. In that
%% case, if that pseudo has been selected for mode2 spills, no spill is inserted
%% after the call.
-module(hipe_range_split).

-export([split/5]).

-compile(inline).

%% -define(DO_ASSERT, 1).
%% -define(DEBUG, 1).
-include("../main/hipe.hrl").

%% Heuristic tuning constants
-define(DEFAULT_MIN_GAIN, 1.1).    % option: range_split_min_gain
-define(DEFAULT_MODE1_FUDGE, 1.1). % option: range_split_mode1_fudge
-define(DEFAULT_WEIGHT_POWER, 2).  % option: range_split_weight_power
-define(WEIGHT_CONST_FUN(Power), math:log(Power)/math:log(100)).
-define(WEIGHT_FUN(Wt, Const), math:pow(Wt, Const)).
-define(HEUR_MAX_TEMPS, 20000).

-type target_cfg()       :: any().
-type target_instr()     :: any().
-type target_temp()      :: any().
-type liveness()         :: any().
-type target_module()    :: module().
-type target_context()   :: any().
-type target()           :: {target_module(), target_context()}.
-type liveset()          :: ordsets:ordset(temp()).
-type temp()             :: non_neg_integer().
-type label()            :: non_neg_integer().

-spec split(target_cfg(), liveness(), target_module(), target_context(),
	    comp_options())
	   -> target_cfg().
split(TCFG0, Liveness, TargetMod, TargetContext, Options) ->
  Target = {TargetMod, TargetContext},
  NoTemps = number_of_temporaries(TCFG0, Target),
  if NoTemps > ?HEUR_MAX_TEMPS ->
      ?debug_msg("~w: Too many temps (~w), falling back on restore_reuse.~n",
		 [?MODULE, NoTemps]),
      hipe_restore_reuse:split(TCFG0, Liveness, TargetMod, TargetContext);
     true ->
      Wts = compute_weights(TCFG0, TargetMod, TargetContext, Options),
      {CFG0, Temps} = convert(TCFG0, Target),
      Avail = avail_analyse(TCFG0, Liveness, Target),
      Defs = def_analyse(CFG0, TCFG0),
      RDefs = rdef_analyse(CFG0),
      PLive = plive_analyse(CFG0),
      {CFG, DUCounts, Costs, DSets0} =
	scan(CFG0, Liveness, PLive, Wts, Defs, RDefs, Avail, Target),
      {DSets, _} = hipe_dsets:to_map(DSets0),
      Renames = decide(DUCounts, Costs, Target, Options),
      rewrite(CFG, TCFG0, Target, Liveness, PLive, Defs, Avail, DSets, Renames,
	      Temps)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal program representation
%%
%% Second pass: Convert cfg to internal representation

-record(cfg, {
	  rpo_labels :: [label()],
	  bbs        :: #{label() => bb()}
	 }).
-type cfg() :: #cfg{}.

cfg_bb(L, #cfg{bbs=BBS}) -> maps:get(L, BBS).

cfg_postorder(#cfg{rpo_labels=RPO}) -> lists:reverse(RPO).

-record(bb, {
	  code     :: [code_elem()],
	  %% If the last instruction of code defines all allocatable registers
	  has_call :: boolean(),
	  succ     :: [label()]
	 }).
-type bb() :: #bb{}.
-type code_elem() :: instr() | mode2_spills() | mode3_restores().

bb_code(#bb{code=Code}) -> Code.
bb_has_call(#bb{has_call=HasCall}) -> HasCall.
bb_succ(#bb{succ=Succ}) -> Succ.

bb_butlast(#bb{code=Code}) ->
  bb_butlast_1(Code).

bb_butlast_1([_Last]) -> [];
bb_butlast_1([I|Is]) -> [I|bb_butlast_1(Is)].

bb_last(#bb{code=Code}) -> lists:last(Code).

-record(instr, {
	  i   :: target_instr(),
	  def :: ordsets:ordset(temp()),
	  use :: ordsets:ordset(temp())
	 }).
-type instr() :: #instr{}.

-record(mode2_spills, {
	  temps :: ordsets:ordset(temp())
	}).
-type mode2_spills() :: #mode2_spills{}.

-record(mode3_restores, {
	  temps :: ordsets:ordset(temp())
	}).
-type mode3_restores() :: #mode3_restores{}.

-spec convert(target_cfg(), target()) -> {cfg(), temps()}.
convert(CFG, Target) ->
  RPO = reverse_postorder(CFG, Target),
  {BBsList, Temps} = convert_bbs(RPO, CFG, Target, #{}, []),
  {#cfg{rpo_labels = RPO,
	bbs = maps:from_list(BBsList)},
   Temps}.

convert_bbs([], _CFG, _Target, Temps, Acc) -> {Acc, Temps};
convert_bbs([L|Ls], CFG, Target, Temps0, Acc) ->
  Succs = hipe_gen_cfg:succ(CFG, L),
  TBB = bb(CFG, L, Target),
  TCode = hipe_bb:code(TBB),
  {Code, Last, Temps} = convert_code(TCode, Target, Temps0, []),
  HasCall = defines_all_alloc(Last#instr.i, Target),
  BB = #bb{code = Code,
	   has_call = HasCall,
	   succ = Succs},
  convert_bbs(Ls, CFG, Target, Temps, [{L,BB}|Acc]).

convert_code([], _Target, Temps, [Last|_]=Acc) ->
  {lists:reverse(Acc), Last, Temps};
convert_code([TI|TIs], Target, Temps0, Acc) ->
  {TDef, TUse} = def_use(TI, Target),
  I = #instr{i = TI,
	     def = ordsets:from_list(reg_names(TDef, Target)),
	     use = ordsets:from_list(reg_names(TUse, Target))},
  Temps = add_temps(TUse, Target, add_temps(TDef, Target, Temps0)),
  convert_code(TIs, Target, Temps, [I|Acc]).

-type temps() :: #{temp() => target_temp()}.
add_temps([], _Target, Temps) -> Temps;
add_temps([T|Ts], Target, Temps) ->
  add_temps(Ts, Target, Temps#{reg_nr(T, Target) => T}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fourth pass: P({DEF}) lattice fwd dataflow (for eliding stores at SPILL
%% splits)
-type defsi() :: #{label() => defseti() | {call, defseti(), defseti()}}.
-type defs()  :: #{label() => defsetf()}.

-spec def_analyse(cfg(), target_cfg()) -> defs().
def_analyse(CFG = #cfg{rpo_labels = RPO}, TCFG) ->
  Defs0 = def_init(CFG),
  def_dataf(RPO, TCFG, Defs0).

-spec def_init(cfg()) -> defsi().
def_init(#cfg{bbs = BBs}) ->
  maps:from_list(
    [begin
       {L, case HasCall of
	     false -> def_init_scan(bb_code(BB), defseti_new());
	     true ->
	       {call, def_init_scan(bb_butlast(BB), defseti_new()),
		defseti_from_ordset((bb_last(BB))#instr.def)}
	   end}
     end || {L, BB = #bb{has_call=HasCall}} <- maps:to_list(BBs)]).

def_init_scan([], Defset) -> Defset;
def_init_scan([#instr{def=Def}|Is], Defset0) ->
  Defset = defseti_add_ordset(Def, Defset0),
  def_init_scan(Is, Defset).

-spec def_dataf([label()], target_cfg(), defsi()) -> defs().
def_dataf(Labels, TCFG, Defs0) ->
  case def_dataf_once(Labels, TCFG, Defs0, 0) of
    {Defs, 0} ->
      def_finalise(Defs);
    {Defs, _Changed} ->
      def_dataf(Labels, TCFG, Defs)
  end.

-spec def_finalise(defsi()) -> defs().
def_finalise(Defs) ->
  maps:from_list([{K, defseti_finalise(BL)}
		  || {K, {call, BL, _}} <- maps:to_list(Defs)]).

-spec def_dataf_once([label()], target_cfg(), defsi(), non_neg_integer())
		    -> {defsi(), non_neg_integer()}.
def_dataf_once([], _TCFG, Defs, Changed) -> {Defs, Changed};
def_dataf_once([L|Ls], TCFG, Defs0, Changed0) ->
  AddPreds =
    fun(Defset1) ->
	lists:foldl(fun(P, Defset2) ->
			defseti_union(defout(P, Defs0), Defset2)
		    end, Defset1, hipe_gen_cfg:pred(TCFG, L))
    end,
  Defset =
    case Defset0 = maps:get(L, Defs0) of
      {call, Butlast, Defout} -> {call, AddPreds(Butlast), Defout};
      _ -> AddPreds(Defset0)
    end,
  Changed = case Defset =:= Defset0 of
	      true  -> Changed0;
	      false -> Changed0+1
	    end,
  def_dataf_once(Ls, TCFG, Defs0#{L := Defset}, Changed).

-spec defout(label(), defsi()) -> defseti().
defout(L, Defs) ->
  case maps:get(L, Defs) of
    {call, _DefButLast, Defout} -> Defout;
    Defout -> Defout
  end.

-spec defbutlast(label(), defs()) -> defsetf().
defbutlast(L, Defs) -> maps:get(L, Defs).

-spec defseti_new() -> defseti().
-spec defseti_union(defseti(), defseti()) -> defseti().
-spec defseti_add_ordset(ordsets:ordset(temp()), defseti()) -> defseti().
-spec defseti_from_ordset(ordsets:ordset(temp())) -> defseti().
-spec defseti_finalise(defseti()) -> defsetf().
-spec defsetf_member(temp(), defsetf()) -> boolean().
-spec defsetf_intersect_ordset(ordsets:ordset(temp()), defsetf())
			      -> ordsets:ordset(temp()).

-type defseti() :: bitord().
defseti_new() -> bitord_new().
defseti_union(A, B) -> bitord_union(A, B).
defseti_add_ordset(OS, D) -> defseti_union(defseti_from_ordset(OS), D).
defseti_from_ordset(OS) -> bitord_from_ordset(OS).
defseti_finalise(D) -> bitarr_from_bitord(D).

-type defsetf() :: bitarr().
defsetf_member(E, D) -> bitarr_get(E, D).

defsetf_intersect_ordset([], _D) -> [];
defsetf_intersect_ordset([E|Es], D) ->
  case bitarr_get(E, D) of
    true  -> [E|defsetf_intersect_ordset(Es,D)];
    false ->    defsetf_intersect_ordset(Es,D)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fifth pass: P({DEF}) lattice reverse dataflow (for eliding stores at defines
%% in mode2)
-type rdefsi() :: #{label() =>
		     {call, rdefseti(), [label()]}
		   | {nocall, rdefseti(), rdefseti(), [label()]}}.
-type rdefs() :: #{label() => {final, rdefsetf(), [label()]}}.

-spec rdef_analyse(cfg()) -> rdefs().
rdef_analyse(CFG = #cfg{rpo_labels=RPO}) ->
  Defs0 = rdef_init(CFG),
  PO = rdef_postorder(RPO, CFG, []),
  rdef_dataf(PO, Defs0).

%% Filter out 'call' labels, since they don't change
-spec rdef_postorder([label()], cfg(), [label()]) -> [label()].
rdef_postorder([], _CFG, Acc) -> Acc;
rdef_postorder([L|Ls], CFG, Acc) ->
  case bb_has_call(cfg_bb(L, CFG)) of
    true  -> rdef_postorder(Ls, CFG, Acc);
    false -> rdef_postorder(Ls, CFG, [L|Acc])
  end.

-spec rdef_init(cfg()) -> rdefsi().
rdef_init(#cfg{bbs = BBs}) ->
  maps:from_list(
    [{L, case HasCall of
	   true ->
	     Defin = rdef_init_scan(bb_butlast(BB), rdefseti_empty()),
	     {call, Defin, Succs};
	   false ->
	     Gen = rdef_init_scan(bb_code(BB), rdefseti_empty()),
	     {nocall, Gen, rdefseti_top(), Succs}
	 end}
     || {L, BB = #bb{has_call=HasCall, succ=Succs}} <- maps:to_list(BBs)]).

-spec rdef_init_scan([instr()], rdefseti()) -> rdefseti().
rdef_init_scan([], Defset) -> Defset;
rdef_init_scan([#instr{def=Def}|Is], Defset0) ->
  Defset = rdefseti_add_ordset(Def, Defset0),
  rdef_init_scan(Is, Defset).

-spec rdef_dataf([label()], rdefsi()) -> rdefs().
rdef_dataf(Labels, Defs0) ->
  case rdef_dataf_once(Labels, Defs0, 0) of
    {Defs, 0} ->
      rdef_finalise(Defs);
    {Defs, _Changed} ->
      rdef_dataf(Labels, Defs)
  end.

-spec rdef_finalise(rdefsi()) -> rdefs().
rdef_finalise(Defs) ->
  maps:map(fun(L, V) ->
	       Succs = rsuccs_val(V),
	       Defout0 = rdefout_intersect(L, Defs, rdefseti_top()),
	       {final, rdefset_finalise(Defout0), Succs}
	   end, Defs).

-spec rdef_dataf_once([label()], rdefsi(), non_neg_integer())
		     -> {rdefsi(), non_neg_integer()}.
rdef_dataf_once([], Defs, Changed) -> {Defs, Changed};
rdef_dataf_once([L|Ls], Defs0, Changed0) ->
  #{L := {nocall, Gen, Defin0, Succs}} = Defs0,
  Defin = rdefseti_union(Gen, rdefout_intersect(L, Defs0, Defin0)),
  Defset = {nocall, Gen, Defin, Succs},
  Changed = case Defin =:= Defin0 of
	      true  -> Changed0;
	      false -> Changed0+1
	    end,
  rdef_dataf_once(Ls, Defs0#{L := Defset}, Changed).

-spec rdefin(label(), rdefsi()) -> rdefseti().
rdefin(L, Defs) -> rdefin_val(maps:get(L, Defs)).
rdefin_val({nocall, _Gen, Defin, _Succs}) -> Defin;
rdefin_val({call, Defin, _Succs}) -> Defin.

-spec rsuccs(label(), rdefsi()) -> [label()].
rsuccs(L, Defs) -> rsuccs_val(maps:get(L, Defs)).
rsuccs_val({nocall, _Gen, _Defin, Succs}) -> Succs;
rsuccs_val({call, _Defin, Succs}) -> Succs.

-spec rdefout(label(), rdefs()) -> rdefsetf().
rdefout(L, Defs) ->
  #{L := {final, Defout, _Succs}} = Defs,
  Defout.

-spec rdefout_intersect(label(), rdefsi(), rdefseti()) -> rdefseti().
rdefout_intersect(L, Defs, Init) ->
  lists:foldl(fun(S, Acc) ->
		  rdefseti_intersect(rdefin(S, Defs), Acc)
	      end, Init, rsuccs(L, Defs)).

-type rdefseti() :: bitord() | top.
rdefseti_top() -> top.
rdefseti_empty() -> bitord_new().
-spec rdefseti_from_ordset(ordsets:ordset(temp())) -> rdefseti().
rdefseti_from_ordset(OS) -> bitord_from_ordset(OS).

-spec rdefseti_add_ordset(ordsets:ordset(temp()), rdefseti()) -> rdefseti().
rdefseti_add_ordset(_, top) -> top; % Should never happen in rdef_dataf
rdefseti_add_ordset(OS, D) -> rdefseti_union(rdefseti_from_ordset(OS), D).

-spec rdefseti_union(rdefseti(), rdefseti()) -> rdefseti().
rdefseti_union(top, _) -> top;
rdefseti_union(_, top) -> top;
rdefseti_union(A, B) -> bitord_union(A, B).

-spec rdefseti_intersect(rdefseti(), rdefseti()) -> rdefseti().
rdefseti_intersect(top, D) -> D;
rdefseti_intersect(D, top) -> D;
rdefseti_intersect(A, B) -> bitord_intersect(A, B).

-type rdefsetf() :: {arr, bitarr()} | top.
-spec rdefset_finalise(rdefseti()) -> rdefsetf().
rdefset_finalise(top) -> top;
rdefset_finalise(Ord) -> {arr, bitarr_from_bitord(Ord)}.

%% rdefsetf_top() -> top.
rdefsetf_empty() -> {arr, bitarr_new()}.

-spec rdefsetf_add_ordset(ordsets:ordset(temp()), rdefsetf()) -> rdefsetf().
rdefsetf_add_ordset(_, top) -> top;
rdefsetf_add_ordset(OS, {arr, Arr}) ->
  {arr, lists:foldl(fun bitarr_set/2, Arr, OS)}.

-spec rdef_step(instr(), rdefsetf()) -> rdefsetf().
rdef_step(#instr{def=Def}, Defset) ->
  %% ?ASSERT(not defines_all_alloc(I, Target)),
  rdefsetf_add_ordset(Def, Defset).

-spec ordset_subtract_rdefsetf(ordsets:ordset(temp()), rdefsetf())
			      -> ordsets:ordset(temp()).
ordset_subtract_rdefsetf(_, top) -> [];
ordset_subtract_rdefsetf(OS, {arr, Arr}) ->
  %% Lazy implementation; could do better if OS can grow
  lists:filter(fun(E) -> not bitarr_get(E, Arr) end, OS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Integer sets represented as bit sets
%%
%% Two representations; bitord() and bitarr()
-define(LIMB_IX_BITS,    11).
-define(LIMB_BITS,       (1 bsl ?LIMB_IX_BITS)).
-define(LIMB_IX(Index),  (Index bsr ?LIMB_IX_BITS)).
-define(BIT_IX(Index),   (Index band (?LIMB_BITS - 1))).
-define(BIT_MASK(Index), (1 bsl ?BIT_IX(Index))).

%% bitord(): fast at union/2 and can be compared for equality with '=:='
-type bitord() :: orddict:orddict(non_neg_integer(), 0..((1 bsl ?LIMB_BITS)-1)).

-spec bitord_new() -> bitord().
bitord_new() -> [].

-spec bitord_union(bitord(), bitord()) -> bitord().
bitord_union(Lhs, Rhs) ->
  orddict:merge(fun(_, L, R) -> L bor R end, Lhs, Rhs).

-spec bitord_intersect(bitord(), bitord()) -> bitord().
bitord_intersect([], _) -> [];
bitord_intersect(_, []) -> [];
bitord_intersect([{K, L}|Ls], [{K, R}|Rs]) ->
  [{K, L band R} | bitord_intersect(Ls, Rs)];
bitord_intersect([{LK, _}|Ls], [{RK, _}|_]=Rs) when LK < RK ->
  bitord_intersect(Ls, Rs);
bitord_intersect([{LK, _}|_]=Ls, [{RK, _}|Rs]) when LK > RK ->
  bitord_intersect(Ls, Rs).

-spec bitord_from_ordset(ordsets:ordset(non_neg_integer())) -> bitord().
bitord_from_ordset([]) -> [];
bitord_from_ordset([B|Bs]) ->
  bitord_from_ordset_1(Bs, ?LIMB_IX(B), ?BIT_MASK(B)).

bitord_from_ordset_1([B|Bs], Key, Val) when Key =:= ?LIMB_IX(B) ->
  bitord_from_ordset_1(Bs, Key, Val bor ?BIT_MASK(B));
bitord_from_ordset_1([B|Bs], Key, Val) ->
  [{Key,Val} | bitord_from_ordset_1(Bs, ?LIMB_IX(B), ?BIT_MASK(B))];
bitord_from_ordset_1([], Key, Val) -> [{Key, Val}].

%% bitarr(): fast (enough) at get/2
-type bitarr() :: array:array(0..((1 bsl ?LIMB_BITS)-1)).

-spec bitarr_new() -> bitarr().
bitarr_new() -> array:new({default, 0}).

-spec bitarr_get(non_neg_integer(), bitarr()) -> boolean().
bitarr_get(Index, Array) ->
  Limb = array:get(?LIMB_IX(Index), Array),
  0 =/= (Limb band ?BIT_MASK(Index)).

-spec bitarr_set(non_neg_integer(), bitarr()) -> bitarr().
bitarr_set(Index, Array) ->
  Limb0 = array:get(?LIMB_IX(Index), Array),
  Limb = Limb0 bor ?BIT_MASK(Index),
  array:set(?LIMB_IX(Index), Limb, Array).

-spec bitarr_from_bitord(bitord()) -> bitarr().
bitarr_from_bitord(Ord) ->
  array:from_orddict(Ord, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sixth pass: Partition-local liveness analysis
%%
%% As temps are not spilled when exiting a partition in mode2, only
%% partition-local uses need to be considered when deciding which temps need
%% restoring at partition entry.

-type plive() :: #{label() =>
		     {call, liveset(), [label()]}
		   | {nocall, {liveset(), liveset()}, liveset(), [label()]}}.

-spec plive_analyse(cfg()) -> plive().
plive_analyse(CFG) ->
  Defs0 = plive_init(CFG),
  PO = cfg_postorder(CFG),
  plive_dataf(PO, Defs0).

-spec plive_init(cfg()) -> plive().
plive_init(#cfg{bbs = BBs}) ->
  maps:from_list(
    [begin
       {L, case HasCall of
	     true ->
	       {Gen, _} = plive_init_scan(bb_code(BB)),
	       {call, Gen, Succs};
	     false ->
	       GenKill = plive_init_scan(bb_code(BB)),
	       {nocall, GenKill, liveset_empty(), Succs}
	   end}
     end || {L, BB = #bb{has_call=HasCall, succ=Succs}} <- maps:to_list(BBs)]).

-spec plive_init_scan([instr()]) -> {liveset(), liveset()}.
plive_init_scan([]) -> {liveset_empty(), liveset_empty()};
plive_init_scan([#instr{def=InstrKill, use=InstrGen}|Is]) ->
  {Gen0, Kill0} = plive_init_scan(Is),
  Gen1 = liveset_subtract(Gen0, InstrKill),
  Gen = liveset_union(Gen1, InstrGen),
  Kill1 = liveset_union(Kill0, InstrKill),
  Kill = liveset_subtract(Kill1, InstrGen),
  {Gen, Kill}.

-spec plive_dataf([label()], plive()) -> plive().
plive_dataf(Labels, PLive0) ->
  case plive_dataf_once(Labels, PLive0, 0) of
    {PLive, 0} -> PLive;
    {PLive, _Changed} ->
      plive_dataf(Labels, PLive)
  end.

-spec plive_dataf_once([label()], plive(), non_neg_integer()) ->
			  {plive(), non_neg_integer()}.
plive_dataf_once([], PLive, Changed) -> {PLive, Changed};
plive_dataf_once([L|Ls], PLive0, Changed0) ->
  Liveset =
    case Liveset0 = maps:get(L, PLive0) of
      {call, Livein, Succs} ->
	{call, Livein, Succs};
      {nocall, {Gen, Kill} = GenKill, _OldLivein, Succs} ->
	Liveout = pliveout(L, PLive0),
	Livein = liveset_union(Gen, liveset_subtract(Liveout, Kill)),
	{nocall, GenKill, Livein, Succs}
    end,
  Changed = case Liveset =:= Liveset0 of
	      true  -> Changed0;
	      false -> Changed0+1
	    end,
  plive_dataf_once(Ls, PLive0#{L := Liveset}, Changed).

-spec pliveout(label(), plive()) -> liveset().
pliveout(L, PLive) ->
  liveset_union([plivein(S, PLive) || S <- psuccs(L, PLive)]).

-spec psuccs(label(), plive()) -> [label()].
psuccs(L, PLive) -> psuccs_val(maps:get(L, PLive)).
psuccs_val({call, _Livein, Succs}) -> Succs;
psuccs_val({nocall, _GenKill, _Livein, Succs}) -> Succs.

-spec plivein(label(), plive()) -> liveset().
plivein(L, PLive) -> plivein_val(maps:get(L, PLive)).
plivein_val({call, Livein, _Succs}) -> Livein;
plivein_val({nocall, _GenKill, Livein, _Succs}) ->  Livein.

liveset_empty() -> ordsets:new().
liveset_subtract(A, B) -> ordsets:subtract(A, B).
liveset_union(A, B) -> ordsets:union(A, B).
liveset_union(LivesetList) -> ordsets:union(LivesetList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Third pass: Compute dataflow analyses required for placing mode3
%% spills/restores.
%% Reuse analysis implementation in hipe_restore_reuse.
%% XXX: hipe_restore_reuse has it's own "rdef"; we would like to reuse that one
%% too.
-type avail() :: hipe_restore_reuse:avail().

-spec avail_analyse(target_cfg(), liveness(), target()) -> avail().
avail_analyse(CFG, Liveness, Target) ->
  hipe_restore_reuse:analyse(CFG, Liveness, Target).

-spec mode3_split_in_block(label(), avail()) -> ordsets:ordset(temp()).
mode3_split_in_block(L, Avail) ->
  hipe_restore_reuse:split_in_block(L, Avail).

-spec mode3_block_renameset(label(), avail()) -> ordsets:ordset(temp()).
mode3_block_renameset(L, Avail) ->
  hipe_restore_reuse:renamed_in_block(L, Avail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Seventh pass
%%
%% Compute program space partitioning, collect information required by the
%% heuristic.
-type part_key() :: label().
-type part_dsets() :: hipe_dsets:dsets(part_key()).
-type part_dsets_map() :: #{part_key() => part_key()}.
-type ducounts() :: #{part_key() => ducount()}.

-spec scan(cfg(), liveness(), plive(), weights(), defs(), rdefs(), avail(),
	   target()) -> {cfg(), ducounts(), costs(), part_dsets()}.
scan(CFG0, Liveness, PLive, Weights, Defs, RDefs, Avail, Target) ->
  #cfg{rpo_labels = Labels, bbs = BBs0} = CFG0,
  CFG = CFG0#cfg{bbs=#{}}, % kill reference
  DSets0 = hipe_dsets:new(Labels),
  Costs0 = costs_new(),
  {BBs, DUCounts0, Costs1, DSets1} =
    scan_bbs(maps:to_list(BBs0), Liveness, PLive, Weights, Defs, RDefs, Avail,
	     Target, #{}, Costs0, DSets0, []),
  {RLList, DSets2} = hipe_dsets:to_rllist(DSets1),
  {Costs, DSets} = costs_map_roots(DSets2, Costs1),
  DUCounts = collect_ducounts(RLList, DUCounts0, #{}),
  {CFG#cfg{bbs=maps:from_list(BBs)}, DUCounts, Costs, DSets}.

-spec collect_ducounts([{label(), [label()]}], ducounts(), ducounts())
		      -> ducounts().
collect_ducounts([], _, Acc) -> Acc;
collect_ducounts([{R,Ls}|RLs], DUCounts, Acc) ->
  DUCount = lists:foldl(
	      fun(Key, FAcc) ->
		  ducount_merge(maps:get(Key, DUCounts, ducount_new()), FAcc)
	      end, ducount_new(), Ls),
  collect_ducounts(RLs, DUCounts, Acc#{R => DUCount}).

-spec scan_bbs([{label(), bb()}], liveness(), plive(), weights(), defs(),
	       rdefs(), avail(), target(), ducounts(), costs(), part_dsets(),
	       [{label(), bb()}])
	      -> {[{label(), bb()}], ducounts(), costs(), part_dsets()}.
scan_bbs([], _Liveness, _PLive, _Weights, _Defs, _RDefs, _Avail, _Target,
	 DUCounts, Costs, DSets, Acc) ->
  {Acc, DUCounts, Costs, DSets};
scan_bbs([{L,BB}|BBs], Liveness, PLive, Weights, Defs, RDefs, Avail, Target,
	 DUCounts0, Costs0, DSets0, Acc) ->
  Wt = weight(L, Weights),
  {DSets, Costs5, EntryCode, ExitCode, RDefout, Liveout} =
    case bb_has_call(BB) of
      false ->
	DSets1 = lists:foldl(fun(S, DS) -> hipe_dsets:union(L, S, DS) end,
			     DSets0, bb_succ(BB)),
	{DSets1, Costs0, bb_code(BB), [], rdefout(L, RDefs),
	 liveout(Liveness, L, Target)};
      true ->
	LastI = #instr{def=LastDef} = bb_last(BB),
	LiveBefore = ordsets:subtract(liveout(Liveness, L, Target), LastDef),
	%% We can omit the spill of a temp that has not been defined since the
	%% last time it was spilled
	SpillSet = defsetf_intersect_ordset(LiveBefore, defbutlast(L, Defs)),
	Costs1 = costs_insert(exit, L, Wt, SpillSet, Costs0),
	Costs4 = lists:foldl(fun({S, BranchWt}, Costs2) ->
				 SLivein = livein(Liveness, S, Target),
				 SPLivein = plivein(S, PLive),
				 SWt = weight_scaled(L, BranchWt, Weights),
				 Costs3 = costs_insert(entry1, S, SWt, SLivein, Costs2),
				 costs_insert(entry2, S, SWt, SPLivein, Costs3)
			     end, Costs1, branch_preds(LastI#instr.i, Target)),
	{DSets0, Costs4, bb_butlast(BB), [LastI], rdefsetf_empty(), LiveBefore}
    end,
  Mode3Splits = mode3_split_in_block(L, Avail),
  {RevEntryCode, Restored} = scan_bb_fwd(EntryCode, Mode3Splits, [], []),
  {Code, DUCount, Mode2Spills} =
    scan_bb(RevEntryCode, Wt, RDefout, Liveout, ducount_new(), [], ExitCode),
  DUCounts = DUCounts0#{L => DUCount},
  M2SpillSet = ordsets:from_list(Mode2Spills),
  Costs6 = costs_insert(spill, L, Wt, M2SpillSet, Costs5),
  Mode3Renames = mode3_block_renameset(L, Avail),
  Costs7 = costs_insert(restore, L, Wt, ordsets:intersection(M2SpillSet, Mode3Renames), Costs6),
  Costs8 = costs_insert(restore, L, Wt, ordsets:from_list(Restored), Costs7),
  Costs = add_unsplit_mode3_costs(DUCount, Mode3Renames, L, Costs8),
  scan_bbs(BBs, Liveness, PLive, Weights, Defs, RDefs, Avail, Target, DUCounts,
	   Costs, DSets, [{L,BB#bb{code=Code}}|Acc]).

-spec add_unsplit_mode3_costs(ducount(), ordsets:ordset(temp()), label(), costs())
			     -> costs().
add_unsplit_mode3_costs(DUCount, Mode3Renames, L, Costs) ->
  Unsplit = orddict_without_ordset(Mode3Renames,
				   orddict:from_list(ducount_to_list(DUCount))),
  add_unsplit_mode3_costs_1(Unsplit, L, Costs).

-spec add_unsplit_mode3_costs_1([{temp(),float()}], label(), costs())
			       -> costs().
add_unsplit_mode3_costs_1([], _L, Costs) -> Costs;
add_unsplit_mode3_costs_1([{T,C}|Cs], L, Costs) ->
  add_unsplit_mode3_costs_1(Cs, L, costs_insert(restore, L, C, [T], Costs)).

%% @doc Returns a new orddict without keys in Set and their associated values.
-spec orddict_without_ordset(ordsets:ordset(K), orddict:orddict(K, V))
			    -> orddict:orddict(K, V).
orddict_without_ordset([S|Ss], [{K,_}|_]=Dict) when S < K ->
  orddict_without_ordset(Ss, Dict);
orddict_without_ordset([S|_]=Set, [D={K,_}|Ds]) when S > K ->
  [D|orddict_without_ordset(Set, Ds)];
orddict_without_ordset([_S|Ss], [{_K,_}|Ds]) -> % _S == _K
  orddict_without_ordset(Ss, Ds);
orddict_without_ordset(_, []) -> [];
orddict_without_ordset([], Dict) -> Dict.

%% Scans the code forward, collecting and inserting mode3 restores
-spec scan_bb_fwd([instr()], ordsets:ordset(temp()), ordsets:ordset(temp()),
		  [code_elem()])
		 -> {[code_elem()], ordsets:ordset(temp())}.
scan_bb_fwd([], [], Restored, Acc) -> {Acc, Restored};
scan_bb_fwd([I|Is], SplitHere0, Restored0, Acc0) ->
  #instr{def=Def, use=Use} = I,
  {ToRestore, SplitHere1} =
    lists:partition(fun(R) -> lists:member(R, Use) end, SplitHere0),
  SplitHere = lists:filter(fun(R) -> not lists:member(R, Def) end, SplitHere1),
  Acc =
    case ToRestore of
      [] -> [I | Acc0];
      _  -> [I, #mode3_restores{temps=ToRestore} | Acc0]
    end,
  scan_bb_fwd(Is, SplitHere, ToRestore ++ Restored0, Acc).

%% Scans the code backwards, collecting def/use counts and mode2 spills
-spec scan_bb([code_elem()], float(), rdefsetf(), liveset(), ducount(),
	      [temp()], [code_elem()])
	     -> {[code_elem()], ducount(), [temp()]}.
scan_bb([], _Wt, _RDefout, _Liveout, DUCount, Spills, Acc) ->
  {Acc, DUCount, Spills};
scan_bb([I=#mode3_restores{}|Is], Wt, RDefout, Liveout, DUCount, Spills, Acc) ->
  scan_bb(Is, Wt, RDefout, Liveout, DUCount, Spills, [I|Acc]);
scan_bb([I|Is], Wt, RDefout, Liveout, DUCount0, Spills0, Acc0) ->
  #instr{def=Def,use=Use} = I,
  DUCount = ducount_add(Use, Wt, ducount_add(Def, Wt, DUCount0)),
  Livein = liveness_step(I, Liveout),
  RDefin = rdef_step(I, RDefout),
  %% The temps that would be spilled after I in mode 2
  NewSpills = ordset_subtract_rdefsetf(
		ordsets:intersection(Def, Liveout),
		RDefout),
  ?ASSERT(NewSpills =:= (NewSpills -- Spills0)),
  Spills = NewSpills ++ Spills0,
  Acc1 = case NewSpills of
	   [] -> Acc0;
	   _ -> [#mode2_spills{temps=NewSpills}|Acc0]
	 end,
  scan_bb(Is, Wt, RDefin, Livein, DUCount, Spills, [I|Acc1]).

-spec liveness_step(instr(), liveset()) -> liveset().
liveness_step(#instr{def=Def, use=Use}, Liveout) ->
  ordsets:union(Use, ordsets:subtract(Liveout, Def)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% First pass: compute basic-block weighting

-type weights() :: no_bb_weights
		 | {hipe_bb_weights:bb_weights(), float()}.

-spec weight(label(), weights()) -> float().
weight(L, Weights) -> weight_scaled(L, 1.0, Weights).

-spec compute_weights(target_cfg(), target_module(), target_context(),
		      comp_options()) -> weights().
compute_weights(CFG, TargetMod, TargetContext, Options) ->
  case proplists:get_bool(range_split_weights, Options) of
    false -> no_bb_weights;
    true ->
      {hipe_bb_weights:compute(CFG, TargetMod, TargetContext),
       ?WEIGHT_CONST_FUN(proplists:get_value(range_split_weight_power,
					     Options, ?DEFAULT_WEIGHT_POWER))}
  end.

-spec weight_scaled(label(), float(), weights()) -> float().
weight_scaled(_L, _Scale, no_bb_weights) -> 1.0;
weight_scaled(L, Scale, {Weights, Const}) ->
  Wt0 = hipe_bb_weights:weight(L, Weights) * Scale,
  Wt = erlang:min(erlang:max(Wt0, 0.0000000000000000001), 10000.0),
  ?WEIGHT_FUN(Wt, Const).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Heuristic splitting decision.
%%
%% Decide which temps to split, in which parts, and pick new names for them.
-type spill_mode() :: mode1 % Spill temps at partition exits
		    | mode2 % Spill temps at definitions
		    | mode3.% Spill temps at definitions, restore temps at uses
-type ren() :: #{temp() => {spill_mode(), temp()}}.
-type renames() :: #{label() => ren()}.

-record(heur_par, {
	  mode1_fudge :: float(),
	  min_gain    :: float()
	  }).
-type heur_par() :: #heur_par{}.

-spec decide(ducounts(), costs(), target(), comp_options()) -> renames().
decide(DUCounts, Costs, Target, Options) ->
  Par = #heur_par{
	   mode1_fudge = proplists:get_value(range_split_mode1_fudge, Options,
					     ?DEFAULT_MODE1_FUDGE),
	   min_gain    = proplists:get_value(range_split_min_gain, Options,
					     ?DEFAULT_MIN_GAIN)},
  decide_parts(maps:to_list(DUCounts), Costs, Target, Par, #{}).

-spec decide_parts([{part_key(), ducount()}], costs(), target(),
		   heur_par(), renames())
		  -> renames().
decide_parts([], _Costs, _Target, _Par, Acc) -> Acc;
decide_parts([{Part,DUCount}|Ps], Costs, Target, Par, Acc) ->
  Spills = decide_temps(ducount_to_list(DUCount), Part, Costs, Target, Par,
			#{}),
  decide_parts(Ps, Costs, Target, Par, Acc#{Part => Spills}).

-spec decide_temps([{temp(), float()}], part_key(), costs(), target(),
		   heur_par(), ren())
		  -> ren().
decide_temps([], _Part, _Costs, _Target, _Par, Acc) -> Acc;
decide_temps([{Temp, SpillGain}|Ts], Part, Costs, Target, Par, Acc0) ->
  SpillCost1 = costs_query(Temp, entry1, Part, Costs)
    + costs_query(Temp, exit, Part, Costs),
  SpillCost2 = costs_query(Temp, entry2, Part, Costs)
    + costs_query(Temp, spill, Part, Costs),
  SpillCost3 = costs_query(Temp, restore, Part, Costs),
  Acc =
    %% SpillCost1 =:= 0.0 usually means the temp is local to the partition;
    %% hence no need to split it
    case (SpillCost1 =/= 0.0) %% maps:is_key(Temp, S)
      andalso (not is_precoloured(Temp, Target))
      andalso ((Par#heur_par.min_gain*SpillCost1 < SpillGain)
	       orelse (Par#heur_par.min_gain*SpillCost2 < SpillGain)
	       orelse (Par#heur_par.min_gain*SpillCost3 < SpillGain))
    of
      false -> Acc0;
      true ->
	Mode =
	  if Par#heur_par.mode1_fudge*SpillCost1 < SpillCost2,
	     Par#heur_par.mode1_fudge*SpillCost1 < SpillCost3 ->
	      mode1;
	     SpillCost2 < SpillCost3 ->
	      mode2;
	     true ->
	      mode3
	  end,
	Acc0#{Temp => {Mode, new_reg_nr(Target)}}
  end,
  decide_temps(Ts, Part, Costs, Target, Par, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Eighth pass: Rewrite program performing range splitting.

-spec rewrite(cfg(), target_cfg(), target(), liveness(), plive(), defs(),
	      avail(), part_dsets_map(), renames(), temps())
	     -> target_cfg().
rewrite(#cfg{bbs=BBs}, TCFG, Target, Liveness, PLive, Defs, Avail, DSets,
	Renames, Temps) ->
  rewrite_bbs(maps:to_list(BBs), Target, Liveness, PLive, Defs, Avail, DSets,
	      Renames, Temps, TCFG).

-spec rewrite_bbs([{label(), bb()}], target(), liveness(), plive(), defs(),
		  avail(), part_dsets_map(), renames(), temps(), target_cfg())
		 -> target_cfg().
rewrite_bbs([], _Target, _Liveness, _PLive, _Defs, _Avail, _DSets, _Renames,
	    _Temps, TCFG) ->
  TCFG;
rewrite_bbs([{L,BB}|BBs], Target, Liveness, PLive, Defs, Avail, DSets, Renames,
	    Temps, TCFG0) ->
  Code0Rev = lists:reverse(bb_code(BB)),
  EntryRen = maps:get(maps:get(L,DSets), Renames),
  M3Ren = mode3_block_renameset(L, Avail),
  SubstFun = rewrite_subst_fun(Target, EntryRen, M3Ren),
  Fun = fun(I) -> subst_temps(SubstFun, I, Target) end,
  {Code, TCFG} =
    case bb_has_call(BB) of
      false ->
	Code1 = rewrite_instrs(Code0Rev, Fun, EntryRen, M3Ren, Temps, Target,
			       []),
	{Code1, TCFG0};
      true ->
	CallI0 = hd(Code0Rev),
	Succ = bb_succ(BB),
	{CallTI, TCFG1} = inject_restores(Succ, Target, Liveness, PLive, DSets,
					 Renames, Temps, CallI0#instr.i, TCFG0),
	Liveout1 = liveness_step(CallI0, liveout(Liveness, L, Target)),
	Defout = defbutlast(L, Defs),
	SpillMap = mk_spillmap(EntryRen, Liveout1, Defout, Temps, Target),
	Code1 = rewrite_instrs(tl(Code0Rev), Fun, EntryRen, M3Ren, Temps,
			       Target, []),
	Code2 = lift_spills(lists:reverse(Code1), Target, SpillMap, [CallTI]),
	{Code2, TCFG1}
    end,
  TBB = hipe_bb:code_update(bb(TCFG, L, Target), Code),
  rewrite_bbs(BBs, Target, Liveness, PLive, Defs, Avail, DSets, Renames, Temps,
	      update_bb(TCFG, L, TBB, Target)).

-spec rewrite_instrs([code_elem()], rewrite_fun(), ren(),
		     ordsets:ordset(temp()), temps(), target(),
		     [target_instr()])
		    -> [target_instr()].
rewrite_instrs([], _Fun, _Ren, _M3Ren, _Temps, _Target, Acc) -> Acc;
rewrite_instrs([I|Is], Fun, Ren, M3Ren, Temps, Target, Acc0) ->
  Acc =
    case I of
      #instr{i=TI} -> [Fun(TI)|Acc0];
      #mode2_spills{temps=Mode2Spills} ->
	add_mode2_spills(Mode2Spills, Target, Ren, M3Ren, Temps, Acc0);
      #mode3_restores{temps=Mode3Restores} ->
	add_mode3_restores(Mode3Restores, Target, Ren, Temps, Acc0)
    end,
  rewrite_instrs(Is, Fun, Ren, M3Ren, Temps, Target, Acc).

-spec add_mode2_spills(ordsets:ordset(temp()), target(), ren(),
		       ordsets:ordset(temp()), temps(), [target_instr()])
		      -> [target_instr()].
add_mode2_spills([], _Target, _Ren, _M3Ren, _Temps, Acc) -> Acc;
add_mode2_spills([R|Rs], Target, Ren, M3Ren, Temps, Acc0) ->
  Acc =
    case Ren of
      #{R := {Mode, NewName}} when Mode =:= mode2; Mode =:= mode3 ->
	case Mode =/= mode3 orelse lists:member(R, M3Ren) of
	  false -> Acc0;
	  true ->
	    #{R := T} = Temps,
	    SpillInstr = mk_move(update_reg_nr(NewName, T, Target), T, Target),
	    [SpillInstr|Acc0]
	end;
    #{} ->
	Acc0
  end,
  add_mode2_spills(Rs, Target, Ren, M3Ren, Temps, Acc).

-spec add_mode3_restores(ordsets:ordset(temp()), target(), ren(), temps(),
			 [target_instr()])
			-> [target_instr()].
add_mode3_restores([], _Target, _Ren, _Temps, Acc) -> Acc;
add_mode3_restores([R|Rs], Target, Ren, Temps, Acc) ->
  case Ren of
    #{R := {mode3, NewName}} ->
      #{R := T} = Temps,
      RestoreInstr = mk_move(T, update_reg_nr(NewName, T, Target), Target),
      add_mode3_restores(Rs, Target, Ren, Temps, [RestoreInstr|Acc]);
    #{} ->
      add_mode3_restores(Rs, Target, Ren, Temps, Acc)
  end.

-type rewrite_fun() :: fun((target_instr()) -> target_instr()).
-type subst_fun() :: fun((target_temp()) -> target_temp()).
-spec rewrite_subst_fun(target(), ren(), ordsets:ordset(temp())) -> subst_fun().
rewrite_subst_fun(Target, Ren, M3Ren) ->
  fun(Temp) ->
      Reg = reg_nr(Temp, Target),
      case Ren of
	#{Reg := {Mode, NewName}} ->
	  case Mode =/= mode3 orelse lists:member(Reg, M3Ren) of
	    false -> Temp;
	    true -> update_reg_nr(NewName, Temp, Target)
	  end;
	#{} -> Temp
      end
  end.

-type spillmap() :: [{temp(), target_instr()}].
-spec mk_spillmap(ren(), liveset(), defsetf(), temps(), target())
		 -> spillmap().
mk_spillmap(Ren, Livein, Defout, Temps, Target) ->
  [begin
     Temp = maps:get(Reg, Temps),
     {NewName, mk_move(update_reg_nr(NewName, Temp, Target), Temp, Target)}
   end || {Reg, {mode1, NewName}} <- maps:to_list(Ren),
	  lists:member(Reg, Livein), defsetf_member(Reg, Defout)].

-spec mk_restores(ren(), liveset(), liveset(), temps(), target())
		 -> [target_instr()].
mk_restores(Ren, Livein, PLivein, Temps, Target) ->
  [begin
     Temp = maps:get(Reg, Temps),
     mk_move(Temp, update_reg_nr(NewName, Temp, Target), Target)
   end || {Reg, {Mode, NewName}} <- maps:to_list(Ren),
	  (       (Mode =:= mode1 andalso lists:member(Reg, Livein ))
	   orelse (Mode =:= mode2 andalso lists:member(Reg, PLivein)))].

-spec inject_restores([label()], target(), liveness(), plive(),
		      part_dsets_map(), renames(), temps(), target_instr(),
		      target_cfg())
		     -> {target_instr(), target_cfg()}.
inject_restores([], _Target, _Liveness, _PLive, _DSets, _Renames, _Temps, CFTI,
		TCFG) ->
  {CFTI, TCFG};
inject_restores([L|Ls], Target, Liveness, PLive, DSets, Renames, Temps, CFTI0,
		TCFG0) ->
  Ren = maps:get(maps:get(L,DSets), Renames),
  Livein = livein(Liveness, L, Target),
  PLivein = plivein(L, PLive),
  {CFTI, TCFG} =
    case mk_restores(Ren, Livein, PLivein, Temps, Target) of
      [] -> {CFTI0, TCFG0}; % optimisation
      Restores ->
	RestBBLbl = new_label(Target),
	Code = Restores ++ [mk_goto(L, Target)],
	CFTI1 = redirect_jmp(CFTI0, L, RestBBLbl, Target),
	TCFG1 = update_bb(TCFG0, RestBBLbl, hipe_bb:mk_bb(Code), Target),
	{CFTI1, TCFG1}
    end,
  inject_restores(Ls, Target, Liveness, PLive, DSets, Renames, Temps, CFTI,
		  TCFG).

%% Heuristic. Move spills up until we meet the edge of the BB or a definition of
%% that temp.
-spec lift_spills([target_instr()], target(), spillmap(), [target_instr()])
		 -> [target_instr()].
lift_spills([], _Target, SpillMap, Acc) ->
  [SpillI || {_, SpillI} <- SpillMap] ++ Acc;
lift_spills([I|Is], Target, SpillMap0, Acc) ->
  Def = reg_defines(I, Target),
  {Spills0, SpillMap} =
    lists:partition(fun({Reg,_}) -> lists:member(Reg, Def) end, SpillMap0),
  Spills = [SpillI || {_, SpillI} <- Spills0],
  lift_spills(Is, Target, SpillMap, [I|Spills ++ Acc]).

reg_defines(I, Target) ->
  reg_names(defines(I,Target), Target).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Costs ADT
%%
%% Keeps track of cumulative cost of spilling temps in particular partitions
%% using particular spill modes.
-type cost_map() :: #{[part_key()|temp()] => float()}.
-type cost_key() :: entry1 | entry2 | exit | spill | restore.
-record(costs, {entry1  = #{} :: cost_map()
	       ,entry2  = #{} :: cost_map()
	       ,exit    = #{} :: cost_map()
	       ,spill   = #{} :: cost_map()
	       ,restore = #{} :: cost_map()
	       }).
-type costs() :: #costs{}.

-spec costs_new() -> costs().
costs_new() -> #costs{}.

-spec costs_insert(cost_key(), part_key(), float(), liveset(), costs())
		  -> costs().
costs_insert(entry1, A, Weight, Liveset, Costs=#costs{entry1=Entry1}) ->
  Costs#costs{entry1=costs_insert_1(A, Weight, Liveset, Entry1)};
costs_insert(entry2, A, Weight, Liveset, Costs=#costs{entry2=Entry2}) ->
  Costs#costs{entry2=costs_insert_1(A, Weight, Liveset, Entry2)};
costs_insert(exit, A, Weight, Liveset, Costs=#costs{exit=Exit}) ->
  Costs#costs{exit=costs_insert_1(A, Weight, Liveset, Exit)};
costs_insert(spill, A, Weight, Liveset, Costs=#costs{spill=Spill}) ->
  Costs#costs{spill=costs_insert_1(A, Weight, Liveset, Spill)};
costs_insert(restore, A, Weight, Liveset, Costs=#costs{restore=Restore}) ->
  Costs#costs{restore=costs_insert_1(A, Weight, Liveset, Restore)}.

costs_insert_1(A, Weight, Liveset, CostMap0) when is_float(Weight) ->
  lists:foldl(fun(Live, CostMap1) ->
		  map_update_counter([A|Live], Weight, CostMap1)
	      end, CostMap0, Liveset).

-spec costs_map_roots(part_dsets(), costs()) -> {costs(), part_dsets()}.
costs_map_roots(DSets0, Costs) ->
  {Entry1,  DSets1} = costs_map_roots_1(DSets0, Costs#costs.entry1),
  {Entry2,  DSets2} = costs_map_roots_1(DSets1, Costs#costs.entry2),
  {Exit,    DSets3} = costs_map_roots_1(DSets2, Costs#costs.exit),
  {Spill,   DSets4} = costs_map_roots_1(DSets3, Costs#costs.spill),
  {Restore, DSets}  = costs_map_roots_1(DSets4, Costs#costs.restore),
  {#costs{entry1=Entry1,entry2=Entry2,exit=Exit,spill=Spill,restore=Restore},
   DSets}.

costs_map_roots_1(DSets0, CostMap) ->
  {NewEs, DSets} = lists:mapfoldl(fun({[A|T], Wt}, DSets1) ->
				      {AR, DSets2} = hipe_dsets:find(A, DSets1),
				      {{[AR|T], Wt}, DSets2}
				  end, DSets0, maps:to_list(CostMap)),
  {maps_from_list_merge(NewEs, fun erlang:'+'/2, #{}), DSets}.

maps_from_list_merge([], _MF, Acc) -> Acc;
maps_from_list_merge([{K,V}|Ps], MF, Acc) ->
  maps_from_list_merge(Ps, MF, case Acc of
				 #{K := OV} -> Acc#{K := MF(V, OV)};
				 #{}        -> Acc#{K => V}
			       end).

-spec costs_query(temp(), cost_key(), part_key(), costs()) -> float().
costs_query(Temp, entry1, Part, #costs{entry1=Entry1}) ->
  costs_query_1(Temp, Part, Entry1);
costs_query(Temp, entry2, Part, #costs{entry2=Entry2}) ->
  costs_query_1(Temp, Part, Entry2);
costs_query(Temp, exit, Part, #costs{exit=Exit}) ->
  costs_query_1(Temp, Part, Exit);
costs_query(Temp, spill, Part, #costs{spill=Spill}) ->
  costs_query_1(Temp, Part, Spill);
costs_query(Temp, restore, Part, #costs{restore=Restore}) ->
  costs_query_1(Temp, Part, Restore).

costs_query_1(Temp, Part, CostMap) ->
  Key = [Part|Temp],
  case CostMap of
    #{Key := Wt} -> Wt;
    #{} -> 0.0
  end.

-spec map_update_counter(Key, number(), #{Key => number(), OK => OV})
			-> #{Key := number(), OK => OV}.
map_update_counter(Key, Incr, Map) ->
  case Map of
    #{Key := Orig} -> Map#{Key := Orig + Incr};
    #{}            -> Map#{Key => Incr}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Def and use counting ADT
-type ducount() :: #{temp() => float()}.

-spec ducount_new() -> ducount().
ducount_new() -> #{}.

-spec ducount_add([temp()], float(), ducount()) -> ducount().
ducount_add([], _Weight, DUCount) -> DUCount;
ducount_add([T|Ts], Weight, DUCount0) ->
  DUCount =
    case DUCount0 of
      #{T := Count} -> DUCount0#{T := Count + Weight};
      #{}           -> DUCount0#{T => Weight}
    end,
  ducount_add(Ts, Weight, DUCount).

ducount_to_list(DUCount) -> maps:to_list(DUCount).

-spec ducount_merge(ducount(), ducount()) -> ducount().
ducount_merge(DCA, DCB) when map_size(DCA) < map_size(DCB) ->
  ducount_merge_1(ducount_to_list(DCA), DCB);
ducount_merge(DCA, DCB) when map_size(DCA) >= map_size(DCB) ->
  ducount_merge_1(ducount_to_list(DCB), DCA).

ducount_merge_1([], DUCount) -> DUCount;
ducount_merge_1([{T,AC}|Ts], DUCount0) ->
  DUCount =
    case DUCount0 of
      #{T := BC} -> DUCount0#{T := AC + BC};
      #{}        -> DUCount0#{T => AC}
    end,
  ducount_merge_1(Ts, DUCount).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Target module interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TGT_IFACE_0(N), N(         {M,C}) -> M:N(         C)).
-define(TGT_IFACE_1(N), N(A1,      {M,C}) -> M:N(A1,      C)).
-define(TGT_IFACE_2(N), N(A1,A2,   {M,C}) -> M:N(A1,A2,   C)).
-define(TGT_IFACE_3(N), N(A1,A2,A3,{M,C}) -> M:N(A1,A2,A3,C)).

?TGT_IFACE_2(bb).
?TGT_IFACE_1(def_use).
?TGT_IFACE_1(defines).
?TGT_IFACE_1(defines_all_alloc).
?TGT_IFACE_1(is_precoloured).
?TGT_IFACE_1(mk_goto).
?TGT_IFACE_2(mk_move).
?TGT_IFACE_0(new_label).
?TGT_IFACE_0(new_reg_nr).
?TGT_IFACE_1(number_of_temporaries).
?TGT_IFACE_3(redirect_jmp).
?TGT_IFACE_1(reg_nr).
?TGT_IFACE_1(reverse_postorder).
?TGT_IFACE_2(subst_temps).
?TGT_IFACE_3(update_bb).
?TGT_IFACE_2(update_reg_nr).

branch_preds(Instr, {TgtMod,TgtCtx}) ->
  merge_sorted_preds(lists:keysort(1, TgtMod:branch_preds(Instr, TgtCtx))).

livein(Liveness, L, Target={TgtMod,TgtCtx}) ->
  ordsets:from_list(reg_names(TgtMod:livein(Liveness, L, TgtCtx), Target)).

liveout(Liveness, L, Target={TgtMod,TgtCtx}) ->
  ordsets:from_list(reg_names(TgtMod:liveout(Liveness, L, TgtCtx), Target)).

merge_sorted_preds([]) -> [];
merge_sorted_preds([{L, P1}, {L, P2}|LPs]) ->
  merge_sorted_preds([{L, P1+P2}|LPs]);
merge_sorted_preds([LP|LPs]) -> [LP|merge_sorted_preds(LPs)].

reg_names(Regs, {TgtMod,TgtCtx}) ->
  [TgtMod:reg_nr(X,TgtCtx) || X <- Regs].
