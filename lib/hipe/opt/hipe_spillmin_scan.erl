%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%% ===========================================================================
%% Copyright (c) 2002 by Niklas Andersson, Andreas Lundin, and Erik Johansson.
%% ===========================================================================
%%  Module   :	hipe_spillmin_scan
%%  Purpose  :  Optimizes the number of stack slots used by using a
%%              "linear-scan algorithm" to allocate stack slots.
%%  Notes    :  * This is a simplified implementation of 
%%                "Linear Scan Register Allocation" by 
%%                Massimiliano Poletto & Vivek Sarkar described in
%%                ACM TOPLAS Vol 21, No 5, September 1999.
%%
%%              * This implementation is target-independent and
%%                requires a target specific interface module
%%                as argument.  
%% 
%%              * Based on the hipe_ls_regalloc module by Erik Johansson
%%
%%  History  :  * 2002-04-01, NA & AL: Created
%%              * 2002-10-08, Happi: Cleanup and speedup
%% ============================================================================
%% Exported functions (short description):
%%   stackalloc(CFG, StackSlots, SpillIndex, Options, Target, TempMap) -> 
%%                    {Coloring, NumberOfSpills}
%%    Takes a CFG and the TempMap from register allocation and returns 
%%    a coloring of stack slots.  
%%    StackSlots should be a list of used stack slots, usually empty at
%%    first call to function.
%%    SpillIndex is the the first position we will spill to, usually 0.
%%    TempMap is the TempMap from the register allocation
%%
%%    The Coloring will be in the form of the "allocation datastructure"
%%    described below, that is, a list of tuples on the form
%%      {Name, {spill, SpillIndex}}
%%    The NumberOfSpills is either 0 indicating no spill or the 
%%    SpillIndex of the last spilled register.
%%
%%  mapmerge
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_spillmin_scan).

-export([stackalloc/6]).

%%-define(DEBUG, 1).
-define(HIPE_INSTRUMENT_COMPILER, true).

%%----------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("../flow/cfg.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% stackalloc(CFG, StackSlots,  SpillIndex, Options, Target, TempMap) 
%%   Calculates an allocation of stack slots using a linear_scan algorithm.
%%   There are three steps in the algorithm:
%%    1. Calculate live-ranges for all spilled temporaries.
%%    2. Calculate live-intervals for each temporary.
%%       The live interval consists of a start position and a end position
%%       these are the first definition and last use of the temporary 
%%       given as instruction numbers in a breadth-first traversal of the
%%       control-flow-graph.
%%    3. Do a linear scan allocation over the live intervals.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec stackalloc(#cfg{}, [_], non_neg_integer(),
		 comp_options(), module(), hipe_temp_map()) ->
                                {hipe_spill_map(), non_neg_integer()}.

stackalloc(CFG, StackSlots, SpillIndex, Options, Target, TempMap) ->
  ?debug_msg("LinearScan: ~w\n", [erlang:statistics(runtime)]),
  %% Step 1: Calculate liveness (Call external implementation.)
  Liveness = liveness(CFG, Target),
  ?debug_msg("liveness (done)~w\n", [erlang:statistics(runtime)]),
  USIntervals = calculate_intervals(CFG, Liveness, Options,
				    Target, TempMap),
  %% ?debug_msg("intervals (done) ~w\n", [erlang:statistics(runtime)]),
  Intervals = sort_on_start(USIntervals),
  ?debug_msg("sort intervals (done) ~w\n", [erlang:statistics(runtime)]),
  ?debug_msg("Intervals ~w\n", [Intervals]),
  ?debug_msg("No intervals: ~w\n", [length(Intervals)]),
  ?debug_msg("count intervals (done) ~w\n", [erlang:statistics(runtime)]),
  Allocation = allocate(Intervals, StackSlots, SpillIndex, Target),
  ?debug_msg("allocation (done) ~w\n", [erlang:statistics(runtime)]),
  Allocation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%        Step 2: Calculate live-intervals for each temporary.        %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% calculate_intervals(CFG, Liveness, Options, Target, TempMap)
%%  CFG: The Control-Flow Graph.
%%  Liveness: A map of live-in and live-out sets for each Basic-Block.
%%  TempMap: The TempMap from the register allocation
%%
%%  This function will only consider the intervals of the temporaries
%%  that have been spilled during register allocation, and will ignore 
%%  all other.
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
calculate_intervals(CFG, Liveness, _Options, Target, TempMap) ->
  Interval = empty_interval(Target:number_of_temporaries(CFG)),
  Worklist = Target:reverse_postorder(CFG),
  intervals(Worklist, Interval, 1, CFG, Liveness, Target, TempMap).

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% intervals(WorkList, Intervals, InstructionNr,
%%           CFG, Liveness, Target, TempMap)
%%  WorkList: List of BB-names to handle.
%%  Intervals: Intervals seen so far (sorted on register names).
%%  InstructionNr: The number of examined instructions.
%%  CFG: The Control-Flow Graph.
%%  Liveness: A map of live-in and live-out sets for each Basic-Block.
%%  Target: The backend for which we generate native code.
%%  TempMap: The TempMap from the register allocation
%%
%%  This function will only consider the intervals of the temporaries
%%  that have been spilled during register allocation, and will ignore 
%%  all other.
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
intervals([L|ToDO], Intervals, InstructionNr, CFG, Liveness, Target, 
	  TempMap) ->
  ?debug_msg("Block ~w\n", [L]),
  %% Add all variables that are live at the entry of this block
  %% to the interval data structure.

  %% Only consider spilled temporaries in LiveIn
  LiveIn = [X || X <- livein(Liveness, L, Target), 
		 hipe_temp_map:is_spilled(X, TempMap)],
  Intervals2 = add_def_point(LiveIn, InstructionNr, Intervals),

  %% Only consider spilled temporaries in LiveOut
  LiveOut = [X2 || X2 <- liveout(Liveness, L, Target), 
		   hipe_temp_map:is_spilled(X2, TempMap)],
  ?debug_msg("In ~w -> Out ~w\n", [LiveIn, LiveOut]),
  
  %% Traverse this block instruction by instruction and add all
  %% uses and defines to the intervals.
  Code = hipe_bb:code(bb(CFG, L, Target)),
  {Intervals3, NewINr} = traverse_block(Code, InstructionNr+1,
					Intervals2, Target, TempMap),
  
  %% Add end points for the temporaries that are in the live-out set.
  Intervals4 = add_use_point(LiveOut, NewINr+1, Intervals3),
  
  intervals(ToDO, Intervals4, NewINr+1, CFG, Liveness, Target, TempMap);
intervals([], Intervals, _, _, _, _, _) -> 
  %% Return the calculated intervals
  interval_to_list(Intervals).
  %% Intervals.

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% traverse_block(Code, InstructionNo, Intervals, Unchanged) 
%%  Examine each instruction in the Code:
%%   For each temporary T used or defined by instruction number N:
%%    extend the interval of T to include N.
%%  TempMap: The TempMap from the register allocation
%%
%%  This function will only consider the the instruction that have temporaries
%%  that have been spilled during register allocation, and will ignore 
%%  all other.
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

traverse_block([Instruction|Is], InstrNo, Intervals, Target, TempMap) ->
  %% Get used temps.
  %% Only consider spilled temporaries in the Use set.
  UsesSet = [X || X <- uses(Instruction, Target), 
		  hipe_temp_map:is_spilled(X, TempMap)],
  %% Get defined temps.
  %% Only consider spilled temporaries in the Def set.
  DefsSet = [X2 || X2 <- defines(Instruction, Target), 
		   hipe_temp_map:is_spilled(X2, TempMap)],
  %% Only consider those temps that starts or ends their lifetime
  %% within the basic block (that is remove all Unchanged temps).
  Intervals1 = add_def_point( DefsSet, InstrNo, Intervals),
  %% Extend the intervals for these temporaries to include InstrNo.
  Intervals2 = add_use_point(UsesSet, InstrNo, Intervals1),
  %% Handle the next instruction.
  traverse_block(Is, InstrNo+1, Intervals2, Target, TempMap);
traverse_block([], InstrNo, Intervals, _, _) -> 
  %% Return the new intervals and the number of the next instruction.
  {Intervals,InstrNo}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%    Step 3. Do a linear scan allocation over the live intervals.    %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% allocate(Intervals, PhysicalRegisters, Target)
%%
%% This function performs the linear scan algorithm.
%%  Intervals contains the start and stop position of each spilled temporary,
%%            sorted on increasing startpositions
%%  StackSlots is a list of available Stack slots to use. If they run out a
%%  new stack slot is allocated from an (in theory) infinite domain.
%%
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
allocate(Intervals, StackSlots, SpillIndex, Target) ->
  AllocatedSlots = empty_allocation(),
  allocate(Intervals, StackSlots, [], AllocatedSlots, SpillIndex, Target).

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% allocate(Intervals, Free, Active, Allocated, SpillIndex, Target) 
%%  Iterates on each temporary interval.
%%   Intervals: The list of temporary intervals.
%%   Free: Currently available stack slots.
%%   Active: Currently used stack slots (sorted on increasing 
%%            interval enpoints)
%%   Allocated: The mapping of register names to spill positions.
%%   SpillIndex: The number of spilled registers. 
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
allocate([TempInt|TIS], Free, Active, Alloc, SpillIndex,  Target) ->
  %% Remove from the active list those temporaries whose interval 
  %% ends before the start of the current interval.
  {NewActive, NewFree} = 
    expire_old_intervals(Active, startpoint(TempInt), Free, Target),
  %% Get the name of the temp in the current interval.
  Temp = reg(TempInt),
  case NewFree of
    [] -> 
      %% There are no free spill slots, so we allocate a new one
      NewSpillIndex = SpillIndex+1,
      NewAlloc = spillalloc(Temp, SpillIndex, Alloc),
      NewActive2 = add_active(endpoint(TempInt), SpillIndex, NewActive),
      allocate(TIS, NewFree, NewActive2, NewAlloc, NewSpillIndex, Target);
    [FreeSpillslot | Spillslots] ->
      %% The spill slot FreeSpillSlot is available, let's use it.
      allocate(TIS, Spillslots,
	       add_active(endpoint(TempInt), FreeSpillslot, NewActive),
	       spillalloc(Temp, FreeSpillslot, Alloc),
	       SpillIndex, Target)
  end;
allocate([], _, _, Alloc, SpillIndex, _) -> 
  %% No more register intervals to handle;
  %% return the result sorted on regnames.
  {lists:sort(Alloc), SpillIndex}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% expire_old_intervals(ActiveTemps, CurrentPos, FreeRegisters) 
%%   Remove all temporaries that have live-ranges that ends before the
%%   current position from the active list and put them into the free
%%   list instead.
%%
%% ---------------------------------------------------------------------
expire_old_intervals([Act|Acts] = AllActives, CurrentPos, Free, Target) ->
  %% Does the live-range of the first active register end before 
  %% the current position?

  %% We expand multimove before regalloc, ignore the next 2 lines.
  %%  %% We don't free registers that end at the current position,
  %%  %% since a multimove can decide to do the moves in another order...
  case active_endpoint(Act) =< CurrentPos of
    true -> %% Yes -> Then we can free that register.
      Spillslot = active_spillslot(Act),
      %% Add the spillslot to the free pool.
      NewFree = [Spillslot|Free],
      %% Here we could try appending the register to get a more
      %% widespread use of registers.
      %% Free ++ [active_spillslot(Act)]);
      expire_old_intervals(Acts, CurrentPos, NewFree, Target);
    false -> 
      %% No -> Then we cannot free any more temporaries.
      %%       (Since they are sorted on endpoints...)    
      {AllActives, Free}
  end;
expire_old_intervals([], _, Free, _) ->
  {[], Free}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%                   D A T A   S T R U C T U R E S                    %%
%%                                &                                   %%
%%               A U X I L I A R Y   F U N C T I O N S                %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The "allocation datastructure"
%%
%% This is an order list of register names paired with their allocations.
%%  {Name, Allocation}
%% Since we are only dealing with spills, the allocation will look like:
%%  {spill, SpillIndex}
%%
%% ---------------------------------------------------------------------

empty_allocation() -> [].

spillalloc(Name, N, Allocation) -> [{Name,{spill,N}}|Allocation].

%% spillalloc(Name,N,[{Name,_}|A]) ->
%%   ?debug_msg("Spilled ~w\n",[Name]),
%%   [{Name,{spill,N}}|A];
%% spillalloc(Name,N,[{Name2,Binding}|Bindings]) when Name > Name2 ->
%%   [{Name2,Binding}|spillalloc(Name,N,Bindings)];
%% spillalloc(Name,N,Bindings) ->
%%   [{Name,{spill,N}}|Bindings].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  The active datastructure.
%%   Keeps tracks of currently active (allocated) spill slots.
%%   It is sorted on end points in the intervals
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_active(Endpoint, SpillSlot, [A1={P1,_}|Active]) when P1 < Endpoint ->
  [A1|add_active(Endpoint, SpillSlot, Active)];
add_active(Endpoint, SpillSlot, Active) ->
  [{Endpoint, SpillSlot}|Active].

active_spillslot({_,SpillSlot}) ->
  SpillSlot.

active_endpoint({EndPoint,_}) ->
  EndPoint.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Interval data structure.
%%
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

%% mk_interval(Name, Start, End) ->
%%   {Name, Start, End}.

endpoint({_R,_S,Endpoint}) ->
  Endpoint.

startpoint({_R,Startpoint,_E}) ->
  Startpoint.

reg({RegName,_S,_E}) ->
  RegName.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Intervals data structure.

sort_on_start(I) ->
 lists:keysort(2, I).

-ifdef(gb_intervals).
empty_interval(_) ->
  gb_trees:empty().

interval_to_list(Intervals) ->
  lists:flatten(
    lists:map(
      fun({T, I}) when is_list(I) ->
	  lists:map(
	    fun ({none, End}) -> 
		{T,End,End};
		({Beg, none}) ->
		{T,Beg, Beg}
	    end,
	    I);
	 ({T,{B,E}}) -> {T, B, E}
      end,
      gb_trees:to_list(Intervals))).

add_use_point([Temp|Temps], Pos, Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case gb_trees:lookup(Temp, Intervals) of
      %% This temp has an old interval...
      {value, Value} ->
	%% ... extend it.
	extend_interval(Pos, Value);
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos}
    end,
  %% Add or update the extended interval.
  Intervals2 = gb_trees:enter(Temp, NewInterval, Intervals),
  %% Add the rest of the temporaries.
  add_use_point(Temps, Pos, Intervals2);
add_use_point([], _, I) ->
  %% No more to add return the interval.
  I.

add_def_point([Temp|Temps], Pos, Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case gb_trees:lookup(Temp, Intervals) of
      %% This temp has an old interval...
      {value, Value} ->
	%% ... extend it.
	extend_interval(Pos, Value);
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos}
    end,
  %% Add or update the extended interval.
  Intervals2 = gb_trees:enter(Temp, NewInterval, Intervals),
  %% Add the rest of the temporaries.
  add_def_point(Temps, Pos, Intervals2);
add_def_point([], _, I) ->
  %% No more to add return the interval.
  I.

extend_interval(Pos, {Beginning, End}) ->
  %% If this position occurs before the beginning of the interval,
  %% then extend the beginning to this position.
  NewBeginning = erlang:min(Pos, Beginning),
  %% If this position occurs after the end of the interval, then
  %% extend the end to this position.
  NewEnd = erlang:max(Pos, End),
  {NewBeginning, NewEnd}.

extend_def_interval(Pos, {Beginning, End}) ->
  %% If this position occurs before the beginning of the interval,
  %% then extend the beginning to this position.
  NewBeginning = erlang:min(Pos, Beginning),
  %% If this position occurs after the end of the interval, then
  %% extend the end to this position.
  NewEnd = erlang:max(Pos, End),
  {NewBeginning, NewEnd};
extend_def_interval(Pos, [{Beginning, none}|More]) ->
  [{Pos,none}, {Beginning, none}|More];
extend_def_interval(Pos, Intervals) ->
  {Pos, Pos}.

-else. %% ifdef gb_intervals

empty_interval(N) ->
  hipe_vectors:new(N, none).

interval_to_list(Intervals) ->
  add_indices(hipe_vectors:vector_to_list(Intervals), 0).

add_indices([{B, E}|Xs], N) ->
  [{N, B, E}|add_indices(Xs, N+1)];
add_indices([List|Xs], N) when is_list(List) ->
  flatten(List, N, Xs);
add_indices([none|Xs], N) ->
  add_indices(Xs, N+1);
add_indices([], _N) -> [].

flatten([{none, End}|Rest], N, More) -> 
  [{N,End,End} | flatten(Rest, N, More)];
flatten([{Beg, none}|Rest], N ,More) ->
  [{N,Beg,Beg} | flatten(Rest, N, More)];
flatten([], N, More) ->
  add_indices(More, N+1).

add_use_point([Temp|Temps], Pos, Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case hipe_vectors:get(Intervals, Temp) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos};
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_interval(Pos, Value)
    end,
  %% Add or update the extended interval.
  Intervals2 = hipe_vectors:set(Intervals, Temp, NewInterval),
  %% Add the rest of the temporaries.
  add_use_point(Temps, Pos, Intervals2);
add_use_point([], _, I) ->
  %% No more to add return the interval.
  I.

add_def_point([Temp|Temps], Pos, Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case hipe_vectors:get(Intervals, Temp) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos};
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_interval(Pos, Value)
    end,
  %% Add or update the extended interval.
  Intervals2 = hipe_vectors:set(Intervals, Temp, NewInterval), 
  %% Add the rest of the temporaries.
  add_def_point(Temps, Pos, Intervals2);
add_def_point([], _, I) ->
  %% No more to add return the interval.
  I.

extend_interval(Pos, {Beginning, End})
  when is_integer(Beginning), is_integer(End) ->
  %% If this position occurs before the beginning of the interval,
  %% then extend the beginning to this position.
  NewBeginning = erlang:min(Pos, Beginning),
  %% If this position occurs after the end of the interval, then
  %% extend the end to this position.
  NewEnd = erlang:max(Pos, End),
  {NewBeginning, NewEnd}.

-endif. %% gb_intervals


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to external functions.
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

liveness(CFG, Target) ->
  Target:analyze(CFG).

bb(CFG, L, Target) ->
  Target:bb(CFG, L).

livein(Liveness, L, Target) ->
  regnames(Target:livein(Liveness, L), Target).

liveout(Liveness, L, Target) ->
  regnames(Target:liveout(Liveness, L), Target).

uses(I, Target) ->
  regnames(Target:uses(I), Target).

defines(I, Target) ->
  regnames(Target:defines(I), Target).

regnames(Regs, Target) ->
  [Target:reg_nr(X) || X <- Regs]. 
