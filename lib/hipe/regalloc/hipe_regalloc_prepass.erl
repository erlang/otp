%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
%%    be allocated to a
%%
%% TODO: Partition the IG (without constructing it) into connected components
%% and call the actual register allocator on them individually.

-module(hipe_regalloc_prepass).
-export([regalloc/6]).

%% -ifndef(DEBUG).
%% -compile(inline).
%% -endif.

%%-define(DO_ASSERT, 1).
-include("../main/hipe.hrl").

%% We present a "pseudo-target" to the register allocator we wrap.
%% Note: all arities are +1 as we're currently using the parameterised module
%% facility to store context data.
-export([analyze/2,
	 all_precoloured/1,
	 allocatable/1,
	 args/2,
	 bb/3,
	 breadthorder/2,
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

%% Eww, parameterised module. Can we fix it without having to touch all the
%% register allocators?
-record(?MODULE,
	{target   :: module()
	,sub      :: sub_map() % Translates temp numbers found in CFG and understood by
			       % Target to temp numbers passed to RegAllocMod.
	,inv      :: inv_map() % Translates temp numbers passed to RegAllocMod
			       % to temp numbers found in CFG and understood by
			       % Target
	,max_phys :: temp()    % Exclusive upper bound on physical registers
	}).

-record(cfg,
	{cfg     :: target_cfg()
	,bbs     :: #{label() => hipe_bb:bb(instr())}
	,max_reg :: temp()    % Exclusive upper bound on temp numbers
	,live    :: target_liveness()
	}).

-record(instr,
	{tgt_instr :: target_instr() % Not used, for debugging
	,defuse    :: {[temp()], [temp()]}
	,is_move   :: boolean()
	}).
%% -record(label,
%% 	{no :: label()
%% 	}).
-type instr() :: #instr{} %% | #label{}
		 .

-type target_cfg() :: any().
-type target_instr() :: any().
-type target_temp() :: any().
-type target_reg() :: non_neg_integer().
-type target_liveness() :: any().
-type spillno() :: non_neg_integer().
-type temp() :: non_neg_integer().
-type label() :: non_neg_integer().

-spec regalloc(module(), target_cfg(), spillno(), spillno(), module(),
	       proplists:proplist())
	      -> {hipe_map(), spillno(), target_liveness()}.
regalloc(RegAllocMod, CFG, SpillIndex0, SpillLimit, Target, Options) ->
  Liveness = Target:analyze(CFG),
  {BBs0, LivePseudos, _Unused, SpilledPseudos, SpillIndex} =
    scan_cfg(CFG, Liveness, SpillIndex0, Target),

  {SubMap, InvMap, MaxPhys, MaxR, SubSpillLimit} =
    number_and_map(Target:all_precoloured(), LivePseudos, SpillLimit),
  BBs = transform_cfg(BBs0, SubMap),
  SubMod = #cfg{cfg=CFG, bbs=BBs, max_reg=MaxR, live=Liveness},
  SubTarget = #?MODULE{target=Target, max_phys=MaxPhys, inv=InvMap, sub=SubMap},
  case
    RegAllocMod:regalloc(SubMod, SpillIndex, SubSpillLimit, SubTarget, Options)
  of
    {SubColoring, NewSpillIndex} -> ok;
    {SubColoring, NewSpillIndex, _SubLiveness} -> ok
  end,
  ?ASSERT(check_coloring(SubColoring, SubMod, SubTarget)), % blame the RA ;)
  SpillColors = [{T, {spill, S}} || {T, S} <- maps:to_list(SpilledPseudos)],
  Coloring = translate_coloring(SubColoring, InvMap) ++ SpillColors,
  ?ASSERT(check_coloring(Coloring, CFG, Target)), % Sanity-check
  ?ASSERT(just_as_good_as(RegAllocMod, CFG, SpillIndex0, SpillLimit, Target,
			  Options, Coloring, _Unused)),
  {Coloring, NewSpillIndex, Liveness}.

number_and_map(Phys, Pseud, SpillLimit) ->
  MaxPhys = lists:max(Phys) + 1,
  case Pseud of [] -> ok; _ ->
      case lists:min(Pseud) of % Assertion
	FirstPseudo when FirstPseudo >= MaxPhys -> ok
      end end,
  NrPseuds = length(Pseud),
  MaxR = MaxPhys+NrPseuds,
  PseudNrs = lists:zip(Pseud, lists:seq(MaxPhys, MaxR-1)),
  MapList = lists:zip(Phys, Phys) % Physicals are identity-mapped
    ++ PseudNrs,
  SubMap = maps:from_list(MapList),
  InvMap = maps:from_list([{Fake, Real} || {Real, Fake} <- MapList]),
  LastPseudo = case Pseud of [] -> MaxPhys-1; _ -> lists:max(Pseud) end,
  SubSpillLimit = if SpillLimit > LastPseudo -> MaxR;
		     true -> map_get(SpillLimit, SubMap)
		  end,
  {SubMap, InvMap, MaxPhys, MaxR, SubSpillLimit}.

-record(spill_state,
	{map :: spill_map()
	,ix  :: spillno()
	}).
-type spill_state() :: #spill_state{}.
-type spill_map()   :: #{target_reg() => spillno()}.

-spec scan_cfg(target_cfg(), target_liveness(), spillno(), module())
	      -> {#{label() => [instr()]}
		 ,ordsets:set(target_reg())
		 ,ordsets:set(target_reg())
		 ,spill_map()
		 ,spillno()
		 }.
scan_cfg(CFG, Liveness, SpillIndex0, Target) ->
  State0 = #spill_state{map=#{}, ix=SpillIndex0},
  {BBs, Seen, #spill_state{map=Spill, ix=SpillIndex}} =
    scan_bbs(Target:labels(CFG), CFG, Liveness, #{}, State0, #{}, Target),
  SeenOSet = ordsets:from_list(maps:keys(Seen)),
  SpillOSet = ordsets:from_list(maps:keys(Spill)),
  PhysOSet = ordsets:from_list(Target:all_precoloured()),
  Dead = ordsets:union(SpillOSet, PhysOSet),
  LivePseudos = ordsets:subtract(SeenOSet, Dead),
  {_TMin, _TMax} = Target:var_range(CFG),
  _Unused = ordsets:subtract(lists:seq(_TMin, _TMax),
			     ordsets:union(SeenOSet, PhysOSet)),
  {BBs, LivePseudos, _Unused, Spill, SpillIndex}.

-type seen() :: #{target_reg() => []}. % set

-spec scan_bbs([label()], target_cfg(), target_liveness(), seen(),
	       spill_state(), #{label() => [instr()]}, module())
	      -> {#{label() => [instr()]}, seen(), spill_state()}.
scan_bbs([], _CFG, _Liveness, Seen, State, BBs, _Target) -> {BBs, Seen, State};
scan_bbs([L|Ls], CFG, Liveness, Seen0, State0, BBs, Target) ->
    {Code, _, Seen, State} = scan_bb(hipe_bb:code(Target:bb(CFG, L)),
				     t_liveout(Liveness, L, Target),
				     Seen0, State0, Target),
    scan_bbs(Ls, CFG, Liveness, Seen, State, BBs#{L=>Code}, Target).

t_liveout(Liveness, L, Target) ->
  %% FIXME: unnecessary sort; liveout is sorted, reg_names(...) should be sorted
  %% or consist of a few sorted subsequences (per type)
  ordsets:from_list(reg_names(Target:liveout(Liveness, L), Target)).

-spec reg_names([target_temp()], module()) -> [target_reg()].
reg_names(Regs, Target) ->
  [Target:reg_nr(X) || X <- Regs].

scan_bb([], Live, Seen, State, _Target) -> {[], Live, Seen, State};
scan_bb([I|Is0], Live0, Seen0, State0, Target) ->
  %% We could pass Target in the return tuple to make the stack frames
  %% smaller, but without multireturn opt, it's probably not worth it.
  {Is1, Live1, Seen1, State1} = scan_bb(Is0, Live0, Seen0, State0, Target),
  {TDef, TUse} = Target:def_use(I),
  Def = ordsets:from_list(reg_names(TDef, Target)),
  Use = ordsets:from_list(reg_names(TUse, Target)),
  Live = ordsets:union(Use, ToSpill = ordsets:subtract(Live1, Def)),
  Seen = add_seen(Def, add_seen(Use, Seen1)),
  State =
    case Target:defines_all_alloc(I) of
      false -> State1;
      true -> spill_all(ToSpill, Target, State1)
    end,
  NewI = #instr{tgt_instr=I, defuse={Def, Use}, is_move=Target:is_move(I)},
  {[NewI|Is1], Live, Seen, State}.

add_seen([], Seen) -> Seen;
add_seen([R|Rs], Seen) -> add_seen(Rs, Seen#{R=>[]}).

spill_all([], _Target, State) -> State;
spill_all([R|Rs], Target, State=#spill_state{map=Map, ix=Ix}) ->
  case Target:is_precoloured(R) or maps:is_key(R, Map) of
    true -> spill_all(Rs, Target, State);
    false -> spill_all(Rs, Target, State#spill_state{map=Map#{R=>Ix}, ix=Ix+1})
  end.

transform_cfg(BBs0, SubMap) ->
  maps:map(fun(_, BB) -> transform_bb(BB, SubMap) end, BBs0).

transform_bb(BB, SubMap) ->
  hipe_bb:mk_bb([I#instr{defuse={map_get_all_partial(Def, SubMap),
				 map_get_all_partial(Use, SubMap)}}
		 || I = #instr{defuse={Def,Use}} <- BB]).

-spec translate_coloring(hipe_map(), _) -> hipe_map().
translate_coloring(SubColoring, InvMap) ->
  lists:map(fun({T, P}) -> {map_get(T, InvMap), P} end, SubColoring).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interference graph partitioning
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We partition the program

%% The algorithm considers two kinds of components; those that are local to a
%% basic block, and those that are not. The key is that any basic block belongs
%% to at most two non-local components; one from the beginning to the first
%% split point, and one from the end to the last split point.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Temp map ADT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type sub_map() :: #{target_temp() => temp()}.
-type inv_map() :: #{temp() => target_temp()}.
map_get(Temp, Map) when is_integer(Temp) -> maps:get(Temp, Map).

%% map_get_all(Ts, Map) -> [map_get(T, Map) || T <- Ts].

map_get_all_partial([], _) -> [];
map_get_all_partial([T|Ts], Map) when is_integer(T) ->
  case Map of
    #{T := R} -> [R|map_get_all_partial(Ts, Map)];
    #{} -> map_get_all_partial(Ts, Map)
  end.

-ifdef(DO_ASSERT).
%%%%%%%%%%%%%%%%%%%%
%% Check that the coloring is correct (if the IG is correct):
%%

%% Define these as 'ok' or 'report(X,Y)' depending on how much output you want.
-define(report0(X,Y), ?IF_DEBUG_LEVEL(0,?msg(X, Y),ok)).
-define(report(X,Y),  ?IF_DEBUG_LEVEL(1,?msg(X, Y),ok)). 
-define(report2(X,Y), ?IF_DEBUG_LEVEL(2,?msg(X, Y),ok)). 
-define(report3(X,Y), ?IF_DEBUG_LEVEL(3,?msg(X, Y),ok)).

check_coloring(Coloring, CFG, Target) ->
  ?report0("checking coloring ~p~n",[Coloring]),
  IG = hipe_ig:build(CFG, Target),
  check_cols(hipe_vectors:list(hipe_ig:adj_list(IG)),
	     init_coloring(Coloring, Target)).

init_coloring(Xs, Target) ->
  hipe_temp_map:cols2tuple(Xs, Target).

check_color_of(X, Cols) ->
%%    if
%%	is_precoloured(X) ->
%%	    phys_reg_color(X,Cols);
%%	true ->
  case hipe_temp_map:find(X, Cols) of
    unknown ->
      ?WARNING_MSG("node ~p: color not found~n", [X]),
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

%%%%%%%%%%%%%%%%%%%%
%% Check that no register allocation opportinities were missed due to ?MODULE
%%
just_as_good_as(RegAllocMod, CFG, SpillIndex0, SpillLimit, Target, Options,
		Coloring, Unused) ->
  {CheckColoring, _} = RegAllocMod:regalloc(CFG, SpillIndex0, SpillLimit,
					    Target, Options),
  Now   = lists:sort([vt(C) || C <- Coloring]),
  Check = lists:sort([vt(C) || C={R,_} <- CheckColoring,
			       not ordsets:is_element(R, Unused)]),
  case Now of
    Check -> true;
    _ ->
      io:fwrite(standard_error, "Colorings differ (~w, sub: ~w, full: ~w)!~n"
		%% "Sub:~w~n"
		"Unused: ~w~n"
		"Now:~w~nCorrect:~w~n",
		[Target, count(Coloring), count(CheckColoring),
		 %% SubColoring,
		 Unused,
		 Now -- Check, Check -- Now]),
	false
  end.

count(C) -> {length([[] || {_, {reg, _}} <- C]),
	     length([[] || {_, {spill, _}} <- C])}.

vt({R, {reg, _}}) -> {R, reg};
vt({R, {spill, _}}) -> {R, spill}.
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pseudo-target interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze(Cfg, _ModRec) -> Cfg.
bb(#cfg{bbs=BBs}, Ix, _ModRec) -> maps:get(Ix, BBs).
args(Arity, #?MODULE{target=Target, sub=SubM}) ->
    map_get(Target:args(Arity), SubM).
labels(#cfg{cfg=CFG}, #?MODULE{target=Target}) -> Target:labels(CFG).
livein(#cfg{live=Live}, Lb, #?MODULE{target=Target, sub=SubM}) ->
  map_get_all_partial(reg_names(Target:livein(Live, Lb), Target), SubM). % Should we precompute?
liveout(#cfg{live=Live}, Lb, #?MODULE{target=Target, sub=SubM}) ->
  map_get_all_partial(reg_names(Target:liveout(Live, Lb), Target), SubM). % dito
uses(I, MR) -> element(2, def_use(I, MR)).
defines(I, MR) -> element(1, def_use(I, MR)).
def_use(#instr{defuse=DefUse}, _ModRec) -> DefUse.
is_move(#instr{is_move=IM}, _ModRec) -> IM.
is_fixed(Reg, #?MODULE{target=Target,inv=InvM}) ->
  Target:is_fixed(map_get(Reg, InvM)). % XXX: Is this hot?
is_global(Reg, #?MODULE{target=Target,max_phys=MaxPhys}) when Reg < MaxPhys ->
  Target:is_global(Reg). % assume id-map
is_precoloured(Reg, #?MODULE{max_phys=MaxPhys}) -> Reg < MaxPhys.
reg_nr(Reg, _ModRec) -> Reg. % After mapping (naturally)
non_alloc(#cfg{cfg=CFG}, #?MODULE{target=Target,sub=SubM}) ->
  map_get_all_partial(reg_names(Target:non_alloc(CFG), Target), SubM).
number_of_temporaries(#cfg{max_reg=MaxR}, _ModRec) -> MaxR.
allocatable(#?MODULE{target=Target}) -> Target:allocatable(). % assume id-map
physical_name(Reg, _ModRec) -> Reg. % XXX: is this correct?
all_precoloured(#?MODULE{target=Target}) -> Target:all_precoloured(). % dito
var_range(#cfg{cfg=CFG, max_reg=MaxReg}, #?MODULE{target=Target}) ->
  {0, _} = Target:var_range(CFG),
  {0, MaxReg-1}. % What is the correct answer?

breadthorder(#cfg{cfg=CFG}, #?MODULE{target=Target}) ->
  Target:breadthorder(CFG).
postorder(#cfg{cfg=CFG}, #?MODULE{target=Target}) -> Target:postorder(CFG).
reverse_postorder(#cfg{cfg=CFG}, #?MODULE{target=Target}) ->
  Target:reverse_postorder(CFG).
