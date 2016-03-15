%% -*- Erlang -*-
%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% LIVENESS ANALYSIS
%%
%% Exports:
%% ~~~~~~~
%% analyze(CFG) - returns a liveness analysis of CFG.
%% liveout(Liveness, Label) - returns a set of variables that are live at
%%      exit from basic block named Label.
%% livein(Liveness, Label) - returns a set of variables that are live at
%%      entry to the basic block named Label.
%% livein_from_liveout(Instructions, LiveOut) - Given a list of instructions 
%%      and a liveout-set, returns a set of variables live at the 
%%      first instruction.
%%

-export([analyze/1,
	 livein/2]).
-ifdef(LIVEOUT_NEEDED).
-export([liveout/2]).
-endif.
-ifdef(PRETTY_PRINT).
-export([pp/1]).
-endif.
%%-export([livein_from_liveout/2]).
-ifdef(DEBUG_LIVENESS).
-export([annotate_liveness/2]).
-endif.

-include("../flow/cfg.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface functions that MUST be implemented in the including file
%%
%% cfg_bb(CFG, L) -> BasicBlock, extract a basic block from a cfg.
%% cfg_postorder(CFG) -> [Labels], the labels of the cfg in postorder
%% cfg_succ(CFG, L) -> [Labels], 
%% uses(Instr) ->
%% defines(Instr) ->
%%
%% Plus the following, if basic block annotations are needed
%%
%% cfg_labels(CFG) ->
%% cfg_bb_add(CFG, L, NewBB) ->
%% mk_comment(Text) ->


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The generic liveness analysis
%%

-spec analyze(cfg()) -> gb_trees:tree().

-ifdef(HIPE_LIVENESS_CALC_LARGEST_LIVESET).
analyze(CFG) ->
  PO = cfg_postorder(CFG),
  InitLiveness = liveness_init(init(cfg_labels(CFG), CFG)),
  _Max = case get(hipe_largest_liveset) of
	   undefined ->
	     put(hipe_largest_liveset, 0),
	     0;
	   LL -> LL
	 end,
  Res = merry_go_around(PO, InitLiveness,0),
  case get(hipe_largest_liveset) > _Max of 
    true ->
      io:format("Largest liveset: ~w \n", [get(hipe_largest_liveset)]);
    _ -> ok
  end,
  Res.

-else.

analyze(CFG) ->
  PO = cfg_postorder(CFG),
  InitLiveness = liveness_init(init(PO, CFG)),
  Res = merry_go_around(PO, InitLiveness, 0),
  Res.
-endif.

%%
%% The fixpoint iteration
%%

merry_go_around(Labels, Liveness, Count) ->
  case doit_once(Labels, Liveness, 0) of
    {NewLiveness, 0} -> 
       %% io:format("Iterations ~w~n", [Count]),
       NewLiveness;
    {NewLiveness, _Changed} ->
       merry_go_around(Labels, NewLiveness, Count+1)
  end.

%%
%% One iteration
%%

-ifdef(HIPE_LIVENESS_CALC_LARGEST_LIVESET).
doit_once([], Liveness, Changed) ->
  {Liveness, Changed};
doit_once([L|Ls], Liveness, Changed) ->
  LiveOut = liveout(Liveness, L),
  Kill = ordsets:subtract(LiveOut, kill(L, Liveness)),
  LiveIn = ordsets:union(Kill, gen(L,Liveness)),
  {NewLiveness, ChangedP} = update_livein(L, LiveIn, Liveness),
  Le = length(LiveIn),
  Max = get(hipe_largest_liveset),
  if Le > Max -> put(hipe_largest_liveset, Le);
     true -> true
  end,
  doit_once(Ls, NewLiveness, Changed+ChangedP).

-else.

doit_once([], Liveness, Changed) ->
  {Liveness, Changed};
doit_once([L|Ls], Liveness, Changed) ->
  LiveOut = liveout(Liveness, L),
  Kill = ordsets:subtract(LiveOut, kill(L, Liveness)),
  LiveIn = ordsets:union(Kill, gen(L,Liveness)),
  {NewLiveness, ChangedP} = update_livein(L, LiveIn, Liveness),
  doit_once(Ls, NewLiveness, Changed+ChangedP).
-endif.

%% %%
%% %% Given a list of instructions and liveout, calculates livein
%% %%
%% livein_from_liveout(List, LiveOut) when is_list(List) ->
%%   livein_from_liveout_1(lists:reverse(List), gb_sets:from_list(LiveOut));
%% livein_from_liveout(Instr, LiveOut) ->
%%   livein_from_liveout_1([Instr], gb_sets:from_list(LiveOut)).
%% 
%% livein_from_liveout_1([], LiveOut) ->
%%   gb_sets:to_list(LiveOut);
%% livein_from_liveout_1([I|Is], LiveOut) ->
%%   Def = defines(I),
%%   Use = uses(I),
%%   DefSet = gb_sets:from_list(Def),
%%   UseSet = gb_sets:from_list(Use),
%%   LiveIn = gb_sets:union(gb_sets:difference(LiveOut, DefSet), UseSet),
%%   Le = gb_sets:size(LiveIn),
%%   Max = get(hipe_largest_liveset),
%%   if Le > Max -> put(hipe_largest_liveset, Le);
%%      true -> true
%%   end,
%%   livein_from_liveout_1(Is, LiveIn).

%%
%% updates liveness for a basic block
%%    - returns: {NewLiveness, ChangedP} 
%%    - ChangedP is 0 if the new LiveIn is equal to the old one
%%      otherwise it's 1.
%%

update_livein(Label, NewLiveIn, Liveness) ->
  {GK, LiveIn, Successors} = liveness_lookup(Label, Liveness),
  NewLiveness = liveness_update(Label, {GK, NewLiveIn, Successors}, Liveness),
  if LiveIn =:= NewLiveIn ->
      {NewLiveness, 0};
  true ->
      {NewLiveness, 1}
  end.


%%
%% LiveOut for a block is the union of the successors LiveIn
%%

liveout(Liveness, L) ->
  Succ = successors(L, Liveness),
  case Succ of
    [] ->    % special case if no successors
      liveout_no_succ();
    _ ->
      liveout1(Succ, Liveness)
  end.

liveout1(Labels, Liveness) ->
  liveout1(Labels, Liveness, ordsets:new()).

liveout1([], _Liveness, Live) ->
  Live;
liveout1([L|Ls], Liveness,Live) ->
  liveout1(Ls, Liveness, ordsets:union(livein(Liveness, L), Live)).

successors(L, Liveness) ->
  {_GK, _LiveIn, Successors} = liveness_lookup(L, Liveness),
  Successors.

-spec livein(gb_trees:tree(), _) -> [_].

livein(Liveness, L) ->
  {_GK, LiveIn, _Successors} = liveness_lookup(L, Liveness),
  LiveIn.

kill(L, Liveness) ->
  {{_Gen, Kill}, _LiveIn, _Successors} = liveness_lookup(L, Liveness),
  Kill.

gen(L, Liveness) ->
  {{Gen, _Kill}, _LiveIn, _Successors} = liveness_lookup(L, Liveness),
  Gen.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% init returns a list of: {Label, {{Gen, Kill}, LiveIn, Successors}}
%%    - Label is the name of the basic block.
%%    - Gen is the set of varables that are used by this block.
%%    - Kill is the set of varables that are defined by this block.
%%    - LiveIn is the set of variables that are alive at entry to the
%%      block (initially empty).
%%    - Successors is a list of the successors to the block.

init([], _) ->
  [];
init([L|Ls], CFG) ->
  BB = cfg_bb(CFG, L),
  Code = hipe_bb:code(BB),
  Succ = cfg_succ(CFG, L),
  Transfer = make_bb_transfer(Code, Succ),
  [{L, {Transfer, ordsets:new(), Succ}} | init(Ls, CFG)].


make_bb_transfer([], _Succ) ->
  {ordsets:new(), ordsets:new()};   % {Gen, Kill}
make_bb_transfer([I|Is], Succ) ->
  {Gen, Kill} = make_bb_transfer(Is, Succ),
  InstrGen = ordsets:from_list(uses(I)),
  InstrKill = ordsets:from_list(defines(I)),
  Gen1 = ordsets:subtract(Gen, InstrKill),
  Gen2 = ordsets:union(Gen1, InstrGen),
  Kill1 = ordsets:union(Kill, InstrKill),
  Kill2 = ordsets:subtract(Kill1, InstrGen),
  {Gen2, Kill2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Annotate each basic block with liveness info
%%

-ifdef(DEBUG_LIVENESS).

annotate_liveness(CFG, Liveness) ->
  Labels = cfg_labels(CFG),
  annotate_liveness_bb(Labels, CFG, Liveness).

annotate_liveness_bb([], CFG, _Liveness) ->
  CFG;
annotate_liveness_bb([L|Ls], CFG, Liveness) ->
  BB = cfg_bb(CFG, L),
  Code0 = hipe_bb:code(BB), 
  LiveIn = strip(livein(Liveness, L)),
  LiveOut = strip(liveout(Liveness, L)),
  Code = [mk_comment({live_in, LiveIn}),
	  mk_comment({live_out, LiveOut})
          | Code0],
  NewBB = hipe_bb:code_update(BB, Code),
  NewCFG = cfg_bb_add(CFG, L, NewBB),
  annotate_liveness_bb(Ls, NewCFG, Liveness).

strip([]) ->
   [];
strip([{_,Y}|Xs]) ->
   [Y|strip(Xs)].

-endif.	% DEBUG_LIVENESS


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
liveness_init(List) ->
  liveness_init(List, gb_trees:empty()).

liveness_init([{Lbl, Data}|Left], Acc) ->
  liveness_init(Left, gb_trees:insert(Lbl, Data, Acc));
liveness_init([], Acc) ->
  Acc.
  
liveness_lookup(Label, Liveness) ->
  gb_trees:get(Label, Liveness).
liveness_update(Label, Val, Liveness) ->
  gb_trees:update(Label, Val, Liveness).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% pp/1 pretty prints liveness information for a CFG
%%

-ifdef(PRETTY_PRINT).

-spec pp(cfg()) -> 'ok'.
pp(Cfg) ->
  Liveness = analyze(Cfg),
  Labels = cfg_labels(Cfg),
  ok = print_blocks(Labels, Liveness, Cfg).

print_blocks([Lbl|Rest], Liveness, Cfg) ->
  io:format("~nLivein:", []),
  pp_liveness_info(livein(Liveness, Lbl)),
  io:format("Label ~w:~n" , [Lbl]),
  pp_block(Lbl, Cfg),
  io:format("Liveout:", []),
  pp_liveness_info(liveout(Liveness, Lbl)),
  print_blocks(Rest, Liveness, Cfg);
print_blocks([], _Liveness, _Cfg) ->
  ok.

-endif. % PRETTY_PRINT
