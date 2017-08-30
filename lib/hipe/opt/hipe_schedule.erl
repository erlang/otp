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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			INSTRUCTION SCHEDULER
%%
%% This is a basic ILP cycle scheduler:
%% * set cycle = 0
%% * while ready[cycle] nonempty do
%%   - take x with greatest priority from ready[cycle]
%%   - try to schedule x;
%%     * if scheduling x was possible, 
%%       - reserve resources
%%       - add x to schedule and delete x from dag
%%       - update earliest-time for all successor nodes
%%         as max[earliest[y],cycle+latency[x]]
%%       - if some node y now has no predecessors,
%%         add y to ready[earliest[y]]
%%     * if it was impossible, put x in ready[cycle+1]
%%       (= try again)
%%
%% We use the following data structures:
%% 1. all nodes are numbered and indices used as array keys
%% 2. priority per node can be computed statically or dynamically
%%    * statically: before scheduling, each node gets a priority value
%%    * dynamically: at each cycle, compute priorities for all ready nodes
%% 3. earliest: earliest cycle of issue, starts at 0
%%    and is updated as predecessors issue
%% 4. predecessors: number of predecessors (0 = ready to issue)
%% 5. successors: list of {Latency,NodeID}
%% 6. ready: an array indexed by cycle-time (integer), where
%%    ready nodes are kept.
%% 7. resources: a resource representation (ADT) that answers
%%    certain queries, e.g., "can x be scheduled this cycle"
%%    and "reserve resources for x".
%% 8. schedule: list of scheduled instructions {Instr,Cycle}
%%    in the order of issue
%% 9. instructions: maps IDs back to instructions
%%
%% Inputs:
%% - a list of {ID,Node} pairs (where ID is a unique key)
%% - a dependence list {ID0,Latency,ID1}, which is used to
%%   build the DAG.
%%
%% Note that there is some leeway in how things are represented
%% from here.
%%
%% MODIFICATIONS:
%% - Some basic blocks are not worth scheduling (e.g., GC save/restore code)
%%   yet are pretty voluminous. How do we skip them?
%% - Scheduling should be done at finalization time: when basic block is
%%   linearized and is definitely at Sparc assembly level, THEN reorder
%%   stuff.

-module(hipe_schedule).
-export([cfg/1, est_cfg/1, delete_node/5]).

-include("../sparc/hipe_sparc.hrl").

%%-define(debug1,true).

-define(debug2(Str,Args),ok).
%%-define(debug2(Str,Args),io:format(Str,Args)).

-define(debug3(Str,Args),ok).
%%-define(debug3(Str,Args),io:format(Str,Args)).

-define(debug4(Str,Args),ok).
%%-define(debug4(Str,Args),io:format(Str,Args)).

-define(debug5(Str,Args),ok).
%%-define(debug5(Str,Args),io:format(Str,Args)).

-define(debug(Str,Args),ok).
%%-define(debug(Str,Args),io:format(Str,Args)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cfg
%% Argument    : CFG - the control flow graph 
%% Returns     : CFG - A new cfg with scheduled blocks
%% Description : Takes each basic block and schedules them one by one.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cfg(CFG) ->
    ?debug3("CFG: ~n~p", [CFG]),
    update_all( [ {L, 
		   hipe_bb:mk_bb(
		     block(L,hipe_bb:code(hipe_sparc_cfg:bb(CFG,L))) )}
		 || L <- hipe_sparc_cfg:labels(CFG) ], CFG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : update_all
%% Argument    : Blocks - [{Label, Block}] , a list with labels and new code
%%                                           used for updating the old CFG.
%%               CFG    - The old controlflow graph
%% Returns     : An updated controlflow graph.
%% Description : Just swappes the basic blocks in the CFG to the scheduled one.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_all([],CFG) -> CFG;
update_all([{L,NewB}|Ls],CFG) ->
    update_all(Ls,hipe_sparc_cfg:bb_add(CFG,L,NewB)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

est_cfg(CFG) ->
    update_all([ {L, hipe_bb:mk_bb(est_block(hipe_bb:code(hipe_sparc_cfg:bb(CFG,L))))}
		 || L <- hipe_sparc_cfg:labels(CFG) ], CFG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Provides an estimation of how quickly a block will execute.
%% This is done by chaining all instructions in sequential order
%% by 0-cycle dependences (which means they will never be reordered), 
%% then scheduling the mess.

est_block([]) -> [];
est_block([I]) -> [I];
est_block(Blk) ->
    {IxBlk,DAG} = est_deps(Blk),
    Sch = bb(IxBlk,DAG),
    separate_block(Sch,IxBlk).

est_deps(Blk) ->
    IxBlk = indexed_bb(Blk),
    DAG = deps(IxBlk),
    {IxBlk, chain_instrs(IxBlk,DAG)}.

chain_instrs([{N,_}|Xs],DAG) ->
    chain_i(N,Xs,DAG).

chain_i(_,[],DAG) -> DAG;
chain_i(N,[{M,_}|Xs],DAG) ->
    NewDAG = dep_arc(N,zero_latency(),M,DAG),
    chain_i(M,Xs,NewDAG).

zero_latency() -> 0.

lookup_instr([{N,I}|_], N) -> I;
lookup_instr([_|Xs], N) -> lookup_instr(Xs, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : block
%% Argument    : Instrs - [Instr], list of all the instructions in a basic
%%                                 block.
%% Returns     : A new scheduled block
%% Description : Schedule a basic block
%%
%%               Note: does not consider delay slots!
%%              (another argument for using only annulled delay slots?)
%%               * how do we add delay slots? somewhat tricky to 
%%                 reconcile with the sort of scheduling we consider. 
%%                 (as-early-as-possible)
%%                 => rewrite scheduler into as-late-as-possible?
%%                (=> just reverse the dependence arcs??)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Don't fire up the scheduler if there's no work to do.
block(_, []) ->
    [];
block(_L, [I]) -> 
    case hipe_sparc:is_any_branch(I) of
	true -> [hipe_sparc:nop_create(), I];
	false -> [I]
    end;
block(_L, Blk) ->
    IxBlk = indexed_bb(Blk),
    case IxBlk of
	[{_N, I}] -> % comments and nops may have been removed.
	    case hipe_sparc:is_any_branch(I) of
		true -> [hipe_sparc:nop_create(), I];
		false -> [I]
	    end;
	_ ->
	    Sch = bb(IxBlk, {DAG, _Preds} = deps(IxBlk)),
	    {NewSch, NewIxBlk} = fill_delays(Sch, IxBlk, DAG),
	    X = finalize_block(NewSch, NewIxBlk),
	    debug1_stuff(Blk, DAG, IxBlk, Sch, X),
	    X
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : fill_delays
%% Argument    : Sch - List of {{cycle, C}, {node, N}} : C = current cycle
%%                                                       N = node index
%%               IxBlk - Indexed block [{N, Instr}]
%%               DAG   - Dependence graph 
%% Returns     : {NewSch, NewIxBlk} - vector with new schedule and vector
%%                                    with {N, Instr}
%% Description : Goes through the schedule from back to front looking for
%%               branches/jumps. If one is found fill_del tries to find
%%               an instr to fill the delayslot.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill_delays(Sch, IxBlk, DAG) ->
    NewIxBlk =  hipe_vectors:list_to_vector(IxBlk),
    %% NewSch = hipe_vectors:list_to_vector(Sch),
    NewSch = fill_del(length(Sch), hipe_vectors:list_to_vector(Sch), 
		      NewIxBlk, DAG),
    {NewSch, NewIxBlk}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : fill_del
%% Argument    : N     - current index in the schedule
%%               Sch   - schedule
%%               IxBlk - indexed block
%%               DAG   - dependence graph
%% Returns     : Sch   - New schedule with possibly a delay instr in the last 
%%                       position.
%% Description : If a call/jump is found fill_branch_delay/fill_call_delay
%%                 is called to find a delay-filler.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill_del(N, Sch, _IxBlk, _DAG) when N < 1 -> Sch;
fill_del(N, Sch, IxBlk, DAG) ->
    Index = get_index(Sch, N),
    ?debug2("Index for ~p: ~p~nInstr: ~p~n",
	    [N, Index, get_instr(IxBlk, Index)]),
    NewSch = 
	case get_instr(IxBlk, Index) of
	    #call_link{} ->
		fill_branch_delay(N - 1, N, Sch, IxBlk, DAG);
	    #jmp_link{} ->
		fill_call_delay(N - 1, N, Sch, IxBlk, DAG);
	    #jmp{} ->
		fill_call_delay(N - 1, N, Sch, IxBlk, DAG);
	    #b{} ->
		fill_branch_delay(N - 1, N, Sch, IxBlk, DAG);
	    #br{} ->
		fill_branch_delay(N - 1, N, Sch, IxBlk, DAG);
	    #goto{} ->
		fill_branch_delay(N - 1, N, Sch, IxBlk, DAG);
	    _Other ->
		Sch
	end,
  NewSch.
  %% fill_del(N - 1, NewSch, IxBlk, DAG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : fill_call_delay
%% Argument    : Cand  - index in schedule of delay-candidate
%%               Call  - index in schedule of call
%%               Sch   - schedule vector: < {{cycle,Ci},{node,Nj}}, ... >
%%               IxBlk - block vector:    < {N, Instr1}, {N+1, Instr2} ... >
%%               DAG   - dependence graph
%% Returns     : Sch - new updated schedule.
%% Description : Searches backwards through the schedule trying to find an 
%%               instr without conflicts with the Call-instr.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill_call_delay(Cand, _Call, Sch, _IxBlk, _DAG) when Cand < 1 -> Sch;
fill_call_delay(Cand, Call, Sch, IxBlk, DAG) ->
    CandIndex = get_index(Sch, Cand),
    CallIndex = get_index(Sch, Call),
    CandI = get_instr(IxBlk, CandIndex),
    case move_or_alu(CandI) of
	true ->
	    case single_depend(CandIndex, CallIndex, DAG) of
		false -> % Other instrs depends on Cand ...
		    fill_call_delay(Cand - 1, Call, Sch, IxBlk, DAG);
		
		true ->
		    CallI = get_instr(IxBlk, CallIndex),
		    
		    CandDefs = ordsets:from_list(hipe_sparc:defines(CandI)),
		    %% CandUses = ordsets:from_list(hipe_sparc:uses(CandI)),
		    %% CallDefs = ordsets:from_list(hipe_sparc:defines(CallI)),
		    CallUses = ordsets:from_list(hipe_sparc:uses(CallI)),
		    
		    Args = case CallI of
			       #jmp_link{} ->
				   ordsets:from_list(
				     hipe_sparc:jmp_link_args(CallI));
			       #jmp{} ->
				   ordsets:from_list(hipe_sparc:jmp_args(CallI));
			       #call_link{} ->
				   ordsets:from_list(
				     hipe_sparc:call_link_args(CallI))
			   end,
		    CallUses2 = ordsets:subtract(CallUses, Args),
		    Conflict = ordsets:intersection(CandDefs, CallUses2),
		    %% io:format("single_depend -> true:~n ~p~n, ~p~n,~p~n",[CandI,CallI,DAG]),
		    %% io:format("Cand = ~p~nCall = ~p~n",[CandI,CallI]),
		    %% io:format("CandDefs = ~p~nCallDefs = ~p~n",[CandDefs,CallDefs]),
		    %% io:format("CandUses = ~p~nCallUses = ~p~n",[CandUses,CallUses]),
		    %% io:format("Args = ~p~nCallUses2 = ~p~n",[Args,CallUses2]),
		    %% io:format("Conflict = ~p~n",[Conflict]),
		    
		    case Conflict of 
			[] -> % No conflicts ==> Cand can fill delayslot after Call
			    update_schedule(Cand, Call, Sch);
			_ -> % Conflict: try with preceeding instrs
			    fill_call_delay(Cand - 1, Call, Sch, IxBlk, DAG)
		    end
	    end;
	false ->
	    fill_call_delay(Cand - 1, Call, Sch, IxBlk, DAG)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : fill_branch_delay
%% Argument    : Cand   - index in schedule of delay-candidate
%%               Branch - index in schedule of branch
%%               Sch    - schedule
%%               IxBlk  - indexed block
%%               DAG    - dependence graph
%% Returns     : Sch - new updated schedule.
%% Description : Searches backwards through the schedule trying to find an 
%%               instr without conflicts with the Branch-instr.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill_branch_delay(Cand, _Br, Sch, _IxBlk, _DAG) when Cand < 1 -> Sch;
fill_branch_delay(Cand, Br, Sch, IxBlk, DAG) -> 
    CandIndex = get_index(Sch, Cand),
    BrIndex   = get_index(Sch, Br),
    CandI = get_instr(IxBlk, CandIndex),
    case move_or_alu(CandI) of
	true ->
	    case single_depend(CandIndex, BrIndex, DAG) of
		false -> % Other instrs depends on Cand ...
		    fill_branch_delay(Cand - 1, Br, Sch, IxBlk, DAG);
		
		true ->
		    BrI      = get_instr(IxBlk, BrIndex),
		    CandDefs = ordsets:from_list(hipe_sparc:defines(CandI)),
		    %% CandUses = ordsets:from_list(hipe_sparc:uses(CandI)),
		    %% BrDefs   = ordsets:from_list(hipe_sparc:defines(BrI)),
		    BrUses   = ordsets:from_list(hipe_sparc:uses(BrI)),
		    
		    Conflict = ordsets:intersection(CandDefs, BrUses),
		    %% io:format("single_depend -> true: ~p~n, ~p~n,~p~n", [CandI, BrI, DAG]),
		    %% io:format("Cand = ~p~nBr = ~p~n",[CandI,BrI]),
		    %% io:format("CandDefs = ~p~nBrDefs = ~p~n",[CandDefs,BrDefs]),
		    %% io:format("CandUses = ~p~nBrUses = ~p~n",[CandUses,BrUses]),
		    %% io:format("Conflict = ~p~n",[Conflict]);
		    
		    case Conflict of 
			[] -> % No conflicts ==> 
                              % Cand can fill delayslot after Branch
			    update_schedule(Cand, Br, Sch);
			_ -> % Conflict: try with preceeding instrs
			    fill_branch_delay(Cand - 1, Br, Sch, IxBlk, DAG)
		    end
	    end;
	false ->
	    fill_branch_delay(Cand - 1, Br, Sch, IxBlk, DAG)
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : update_schedule
%% Argument    : From - the position from where to switch indexes in Sch
%%               To   - the position to where to switch indexes in Sch
%%               Sch  - schedule
%% Returns     : Sch - an updated schedule
%% Description : If From is the delay-filler and To is the Call/jump, the 
%%               schedule is updated so From gets index To, To gets index 
%%               To - 1, and the nodes between From and To gets old_index - 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_schedule(To, To, Sch) -> 
    {{cycle, C}, {node, _N} = Node} = hipe_vectors:get(Sch, To-1),
    hipe_vectors:set(Sch, To-1, {{cycle, C+1}, Node});
update_schedule(From, To, Sch) ->
    Temp = hipe_vectors:get(Sch, From-1),
    Sch1 = hipe_vectors:set(Sch, From-1, hipe_vectors:get(Sch, From)),
    update_schedule(From + 1, To, hipe_vectors:set(Sch1, From, Temp)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : single_depend
%% Argument    : N    - Index of the delayslot candidate
%%               M    - Index of the node that N possibly has a single
%%                      depend to.
%%               DAG  - The dependence graph
%% Returns     : true if no other nodes than N os depending on N
%% Description : Checks that no other nodes than M depends on N and that the 
%%               latency between them is zero or 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
single_depend(N, M, DAG) ->
    Deps = hipe_vectors:get(DAG, N-1),
    single_depend(M, Deps).

single_depend(_N, []) -> true;
single_depend(N, [{0, N}]) -> true;
single_depend(N, [{1, N}]) -> true;
single_depend(_N, [{_Lat, _}|_]) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : get_index
%% Argument    : Sch - schedule
%%               N   - index in schedule
%% Returns     : Index - index of the node
%% Description : Returns the index of the node on position N in the schedule.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_index(Sch, N) ->
    {{cycle, _C}, {node, Index}} = hipe_vectors:get(Sch,N-1),
    Index.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : get_instr
%% Argument    : IxBlk - indexed block
%%               N     - index in block
%% Returns     : Instr
%% Description : Returns the instr on position N in the indexed block.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_instr(IxBlk, N) ->
    {_, Instr} = hipe_vectors:get(IxBlk, N-1),
    Instr.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : get_instr
%% Argument    : Sch   - schedule
%%               IxBlk - indexed block
%%               N     - index in schedule
%% Returns     : Instr
%% Description : Returns the instr on position N in the schedule.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_instr(Sch, IxBlk, N) ->
     {{cycle, _C}, {node, Index}} = hipe_vectors:get(Sch, N-1),
     {_, Instr} = hipe_vectors:get(IxBlk, Index-1),
     Instr.

separate_block(Sch,IxBlk) ->
    sep_comments([{C,lookup_instr(IxBlk,N)} || {{cycle,C},{node,N}} <- Sch]).

sep_comments([]) -> [];
sep_comments([{C,I}|Xs]) ->
    [hipe_sparc:comment_create({cycle,C}), I | sep_comments(Xs,C)].

sep_comments([], _) -> [];
sep_comments([{C1,I}|Xs], C0) ->
    if
	C1 > C0 ->
	    [hipe_sparc:comment_create({cycle,C1}),I|sep_comments(Xs,C1)];
	true ->
	    [I|sep_comments(Xs, C0)]
    end.

finalize_block(Sch, IxBlk) ->
    ?debug5("Sch: ~p~nIxBlk: ~p~n",[Sch,IxBlk]),
    finalize_block(1, hipe_vectors:size(Sch), 1, Sch, IxBlk, []).

finalize_block(N, End, _C, Sch, IxBlk, _Instrs) when N =:= End - 1 ->
    NextLast = get_instr(Sch, IxBlk, N),
    Last     = get_instr(Sch, IxBlk, End),
    ?debug5("NextLast: ~p~nLast: ~p~n",[NextLast,Last]),
    case hipe_sparc:is_any_branch(Last) of
	true -> % Couldn't fill delayslot ==> add NOP
	    [NextLast , hipe_sparc:nop_create(), Last];
	false ->  % Last is a delayslot-filler ==> change order...
	    [Last, NextLast]
    end;
finalize_block(N, End, C0, Sch, IxBlk, Instrs) ->
    {{cycle, _C1}, {node, _M}} = hipe_vectors:get(Sch, N-1),
    Instr = get_instr(Sch, IxBlk, N),
    ?debug5("Instr: ~p~n~n",[Instr]),
    [Instr | finalize_block(N + 1, End, C0, Sch, IxBlk, Instrs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : bb
%% Argument    : IxBlk - indexed block
%%               DAG   - {Dag, Preds} where Dag is dependence graph and 
%%                       Preds is number of predecessors for each node.
%% Returns     : Sch 
%% Description : Initializes earliest-list, ready-list, priorities, resources
%%               and so on, and calls the cycle_sched which does the scheduling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bb(IxBlk,DAG) ->
    bb(length(IxBlk), IxBlk, DAG).

bb(N,IxBlk,{DAG, Preds}) ->
    Earliest = init_earliest(N),
    BigArray = N*10,                     % "nothing" is this big :-)
    Ready = hipe_schedule_prio:init_ready(BigArray,Preds),
    I_res = init_instr_resources(N, IxBlk),
    
    Prio = hipe_schedule_prio:init_instr_prio(N,DAG),
    Rsrc = init_resources(BigArray),
    ?debug4("I_res: ~n~p~nPrio: ~n~p~nRsrc: ~n~p~n", [I_res,Prio,Rsrc]),
    ?debug('cycle 1~n',[]),
    Sch = empty_schedule(),
    cycle_sched(1,Ready,DAG,Preds,Earliest,Rsrc,I_res,Prio,Sch,N,IxBlk).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cycle_sched
%% Argument    :  - C is current cycle, 1 or more.
%%                - Ready is an array (Cycle -> [Node])
%%                    yielding the collection of nodes ready to be 
%%                    scheduled in a cycle.
%%                - DAG is an array (Instr -> [{Latency,Instr}])
%%                    represents the dependence DAG.
%%                - Preds is an array (Instr -> NumPreds)
%%                    counts the number of predecessors 
%%                    (0 preds = ready to be scheduled).
%%                - Earl is an array (Instr -> EarliestCycle)
%%                    holds the earliest cycle an instruction can be scheduled.
%%                - Rsrc is a 'resource ADT' that handles scheduler resource 
%%                    management checks whether instruction can be scheduled
%%                    this cycle without a stall.
%%                - I_res is an array (Instr -> Required_resources)
%%                    holds the resources required to schedule an instruction.
%%                - Sch is the representation of the schedule current schedule.
%%                - N is the number of nodes remaining to be scheduled
%%                    tells us when to stop the scheduler.
%%                - IxBlk is the indexed block with instrs
%% Returns     : present schedule
%% Description : Scheduler main loop.
%%               Pick next ready node in priority order for cycle C until 
%%               none remain.
%%                 * check each node if it can be scheduled w/o stalling
%%                 * if so, schedule it
%%                 * otherwise, bump the node to the next cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cycle_sched(C,Ready,DAG,Preds,Earl,Rsrc,I_res,Prio,Sch,N,IxBlk) ->
    case hipe_schedule_prio:next_ready(C,Ready,Prio,IxBlk,DAG,Preds,Earl) of
%  case hipe_schedule_prio:next_ready(C,Ready,Prio,IxBlk) of
	{next,I,Ready1} ->  
	    ?debug('try ~p~n==> ready = ~p~n',[I, Ready1]),
	    case resources_available(C,I,Rsrc,I_res) of
		{yes,NewRsrc} ->
		    ?debug(' scheduled~n==> Rscrs = ~p~n',[NewRsrc]),
		    NewSch = add_to_schedule(I,C,Sch),
		    {ReadyNs,NewDAG,NewPreds,NewEarl} = 
			delete_node(C,I,DAG,Preds,Earl),
		    ?debug("NewPreds : ~p~n",[Preds]),
		    ?debug(' ReadyNs: ~p~n',[ReadyNs]),
		    NewReady = hipe_schedule_prio:add_ready_nodes(ReadyNs,
								  Ready1),
		    ?debug(' New ready: ~p~n',[NewReady]),
		    cycle_sched(C,NewReady,NewDAG,NewPreds,NewEarl,
				NewRsrc,I_res,Prio,NewSch,N-1, IxBlk);
		no ->
		    ?debug(' resource conflict~n',[]),
		    NewReady = hipe_schedule_prio:insert_node(C+1,I,Ready1),
		    cycle_sched(C,NewReady,DAG,Preds,Earl,Rsrc,
				I_res,Prio,Sch,N,IxBlk)
	    end;
	none ->  % schedule next cycle if some node remains
	    if
		N > 0 ->
		    ?debug('cycle ~p~n',[C+1]),
		    cycle_sched(C+1,Ready,DAG,Preds,Earl,
				advance_cycle(Rsrc),
				I_res,Prio,Sch,N, IxBlk);
		true ->
		    present_schedule(Sch)
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : init_earliest
%% Argument    : N - number of instrs
%% Returns     : 
%% Description : 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_earliest(N) ->
    hipe_vectors:new(N,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Schedule is kept reversed until the end.

-define(present_node(I,Cycle),{{cycle,Cycle},{node,I}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : empty_schedule
%% Description : Returns an empty schedule.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_schedule() -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_to_schedule
%% Argument    : I     - instr
%%               Cycle - cycle when I was placed
%%               Sch   - schedule
%% Description : Adds instr to schedule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_to_schedule(I,Cycle,Sch) ->
    [?present_node(I,Cycle)|Sch].

present_schedule(Sch) -> lists:reverse(Sch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to resource manager:
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : init_resources
%% Description : Yields a 'big enough' array mapping (Cycle -> Resources);
%%                this array is called Rsrc below.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_resources(S) ->
    hipe_target_machine:init_resources(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : init_instr_resources
%% Argument    : Nodes - a list of the instructions
%%               N     - is the number of nodes
%% Description : return a vector (NodeID -> Resource_requirements)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_instr_resources(N,Nodes) ->
    hipe_target_machine:init_instr_resources(N,Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : resources_available
%% Argument    : Cycle - the current cycle
%%               I     - the current instruction (index = NodeID)
%%               Rsrc  - a map (Cycle -> Resources)
%%               I_res - maps (NodeID -> Resource_requirements)
%% Description : returns {yes,NewResTab} | no
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resources_available(Cycle,I,Rsrc,I_res) ->
    hipe_target_machine:resources_available(Cycle,I,Rsrc,I_res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : advance_cycle
%% Argument    : Rsrc - resources
%% Description : Returns an empty resources-state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
advance_cycle(Rsrc) ->
    hipe_target_machine:advance_cycle(Rsrc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : delete_node
%% Argument    : Cycle - current cycle
%%               I     - index of instr
%%               DAG   - dependence dag
%%               Preds - array with number of predecessors for nodes
%%               Earl  - array with earliest-times for nodes
%% Returns     : {ReadyNs,NewDAG,NewPreds,NewEarl}
%% Description : Deletes node I and updates earliest times for the rest.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_node(Cycle,I,DAG,Preds,Earl) ->
    Succ = hipe_vectors:get(DAG,I-1),
    NewDAG = hipe_vectors:set(DAG,I-1,scheduled),  % provides debug 'support'
    {ReadyNs,NewPreds,NewEarl} = update_earliest(Succ,Cycle,Preds,Earl,[]),
    ?debug('earliest after ~p: ~p~n',[I,[{Ix+1,V} || {Ix,V} <- hipe_vectors:list(NewEarl)]]),
    {ReadyNs,NewDAG,NewPreds,NewEarl}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : update_earliest
%% Argument    : Succ - successor list
%%               Cycle - current cycle
%%               Preds - predecessors
%%               Earl  - earliest times for nodes
%%               Ready - array with readynodes for cycles
%% Returns     : {Ready,Preds,Earl}
%% Description : Updates the earliest times for nodes and updates number of 
%%               predecessors for nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_earliest([],_Cycle,Preds,Earl,Ready) ->
    {Ready,Preds,Earl};
update_earliest([{Lat,N}|Xs],Cycle,Preds,Earl,Ready) ->
    Old_earl = hipe_vectors:get(Earl,N-1),
    New_earl = erlang:max(Old_earl,Cycle+Lat),
    NewEarl = hipe_vectors:set(Earl,N-1,New_earl),
    Num_preds = hipe_vectors:get(Preds,N-1),
    NewPreds = hipe_vectors:set(Preds,N-1,Num_preds-1),
    if
	Num_preds =:= 0 ->
	    ?debug('inconsistent DAG~n',[]),
	    exit({update_earliest,N});
	Num_preds =:= 1 ->
	    NewReady = [{New_earl,N}|Ready],
	    NewPreds2 = hipe_vectors:set(NewPreds,N-1,0),
	    update_earliest(Xs,Cycle,NewPreds2,NewEarl,NewReady);
	is_integer(Num_preds), Num_preds > 1 ->
	    update_earliest(Xs,Cycle,NewPreds,NewEarl,Ready)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Collect instruction dependences.
%%
%% Three forms:
%% - data/register
%%   * insert RAW, WAR, WAW dependences
%% - memory
%%   * stores serialize memory references
%%   * alias analysis may allow loads to bypass stores
%% - control
%%   * unsafe operations are 'trapped' between branches
%%   * branches are ordered
%%
%% returns { [{Index,Instr}], DepDAG }
%%   DepDAG is defined below.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : deps
%% Argument    : BB - Basic block 
%% Returns     : {IxBB,DAG} - indexed block and dependence graph. DAG consists 
%%                            of both Dag and Preds, where Preds is number
%%                            of predecessors for nodes.
%% Description : Collect instruction dependences.
%%
%%               Three forms:
%%               - data/register
%%                 * insert RAW, WAR, WAW dependences
%%               - memory
%%                 * stores serialize memory references
%%                 * alias analysis may allow loads to bypass stores
%%               - control
%%                 * unsafe operations are 'trapped' between branches
%%                 * branches are ordered
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deps(IxBB) ->
    N = length(IxBB),
    DAG = empty_dag(N),  % The DAG contains both dependence-arcs and 
                         % number of predeccessors...
    {_DepTab,DAG1} = dd(IxBB, DAG),
    DAG2 = md(IxBB, DAG1),
    cd(IxBB, DAG2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : empty_dag
%% Argument    : N - number of nodes
%% Returns     : empty DAG
%% Description : DAG consists of dependence graph and predeccessors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_dag(N) ->
    {hipe_vectors:new(N, []), hipe_vectors:new(N, 0)}. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : indexed_bb
%% Argument    : BB - basic block
%% Returns     : [{N, Instr}]
%% Description : Puts indexes to all instrs of a block, removes comments.
%%               NOP's are also removed because if both sparc_schedule and
%%               sparc_post_schedule options are used, the first pass will
%%               add nop's before the branch if necessary, and these are
%%               removed before scheduling the second pass.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
indexed_bb(BB) ->
    indexed_bb(BB,1).

indexed_bb([],_N) -> [];
indexed_bb([X|Xs],N) ->
    case X of
	#comment{} ->
	    indexed_bb(Xs,N);
	#nop{} ->
	    indexed_bb(Xs,N);
	_Other ->
	    [{N,X}|indexed_bb(Xs,N+1)]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : dep_arc
%% Argument    : N   - Current node 
%%               Lat - Latency from current node to M
%%               M   - The dependent node
%%               DAG - The dependence graph. Consists of both DAG and 
%%                     predeccessors
%% Returns     : A new DAG with the arc added and number of predeccessors for
%%                 M increased.
%% Description : Adds a new arc to the graph, if an older arc goes from N to M
%%               it will be replaced with a new arc {max(OldLat, NewLat), M}.
%%               Number of predeccessors for node M is increased.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dep_arc(N, Lat, M, {Dag,Preds}) -> 
    OldDeps = hipe_vectors:get(Dag, N-1),
    %% io:format("{OldDeps} = {~p}~n",[OldDeps]),
    {NewDeps, Status} = add_arc(Lat, M, OldDeps),
    %% io:format("{NewDeps, Status} = {~p, ~p}~n",[NewDeps, Status]),
    NewDag  = hipe_vectors:set(Dag, N-1, NewDeps),
    NewPreds = case Status of
		   added -> % just increase preds if new arc was added
		       OldPreds = hipe_vectors:get(Preds, M-1),
		       hipe_vectors:set(Preds, M-1, OldPreds + 1);
		   non_added -> 
		       Preds
	       end,
    {NewDag, NewPreds}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_arc
%% Argument    : Lat  - The latency from current node to To.
%%               To   - The instr-id of the node which the dependence goes to
%%               Arcs - The dependecies that are already in the dep-graph
%% Returns     : A dependence graph sorted by To. 
%% Description : A new arc that is added is sorted in the right place, and if
%%               there is already an arc between nodes A and B, the one with 
%%               the greatest latency is chosen.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_arc(Lat,To, []) -> {[{Lat, To}], added};
add_arc(Lat1, To, [{Lat2, To} | Arcs]) ->
    {[{erlang:max(Lat1, Lat2), To} | Arcs], non_added};
add_arc(Lat1,To1, [{Lat2, To2} | Arcs]) when To1 < To2 ->
    {[{Lat1, To1}, {Lat2, To2} | Arcs], added};
add_arc(Lat1 ,To1, [{Lat2, To2} | Arcs]) ->
    {Arcs1, Status} = add_arc(Lat1, To1, Arcs),
    {[{Lat2, To2} | Arcs1], Status}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The register/data dependence DAG of a block is represented
%% as a mapping (Variable -> {NextWriter,NextReaders})
%%  where NextWriter is a pair {Ix,Type}
%%  and NextReaders is a list of pairs {Ix,Type}.
%%
%% Type is used to determine latencies of operations; on the UltraSparc,
%% latencies of arcs (n -> m) are determined by both n and m. (E.g., if
%% n is an integer op and m is a store, then latency is 0; if m is an
%% integer op, it's 1.)

dd([],DAG) -> { empty_deptab(), DAG };
dd([{N,I}|Is],DAG0) ->
    {DepTab,DAG1} = dd(Is,DAG0),
    add_deps(N,I,DepTab,DAG1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_deps
%% Argument    : N         - current node
%%               Instr     - current instr
%%               DepTab    - hashtable with {next-writer, next-readers} for reg
%%               DAG       - dependence graph
%% Returns     : {DepTab, BlockInfo, DAG} - with new values
%% Description : Adds dependencies for node N to the graph. The registers that
%%               node N defines and uses are used for computing the 
%%               dependencies to the following nodes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_deps(N,Instr,DepTab,DAG) ->
    {Ds,Us} = def_use(Instr),
    Type = dd_type(Instr),
    {DepTab1,DAG1} = add_write_deps(Ds,N,Type,DepTab,DAG),
    add_read_deps(Us,N,Type,DepTab1,DAG1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Instructions are classified into symbolic categories,
%% which are subsequently used to determine operation latencies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dd_type(Instr) ->
    case Instr of
	#b{} -> branch;
	%% #br{} -> branch;
	#call_link{} -> branch;
	#jmp_link{} -> branch;
	#jmp{} -> branch;
	#goto{} -> branch;
	#load{} -> load;
	#store{} -> store;
	#alu{} -> alu;
	#move{} -> alu;
	#multimove{} -> 
	    Src = hipe_sparc:multimove_src(Instr),
	    Lat = round(length(Src)/2),
	    {mmove,Lat};
	#sethi{} -> alu;
	#alu_cc{} -> alu_cc;
	%% #cmov_cc{} -> cmov_cc;
	%% #cmov_r{} -> alu;
	#load_atom{} -> alu;
	#load_address{} -> alu;
	#pseudo_enter{} -> pseudo;
	#pseudo_pop{} -> pseudo;
	#pseudo_return{} -> pseudo;
	#pseudo_spill{} -> pseudo;
	#pseudo_unspill{} -> pseudo
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_write_deps
%% Argument    : Defs   - registers that node N defines.
%%               N      - current node
%%               Ty     - the type of current instr
%%               DepTab - Dependence-table
%%               DAG    - The dependence graph.
%% Returns     : {DepTab,DAG} - with new values
%% Description : Adds dependencies to the graph for nodes that depends on the
%%               registers that N defines.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_write_deps([],_N,_Ty,DepTab,DAG) -> {DepTab,DAG};
add_write_deps([D|Ds],N,Ty,DepTab,DAG) ->
    {NewDepTab,NewDAG} = add_write_dep(D,N,Ty,DepTab,DAG),
    add_write_deps(Ds,N,Ty,NewDepTab,NewDAG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_write_dep
%% Description : Updates the dependence table with N as next writer, and
%%               updates the DAG with the dependencies from N to subsequent 
%%               nodes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_write_dep(X,N,Ty,DepTab,DAG) ->
    {NxtWriter,NxtReaders} = lookup(X,DepTab),
    NewDepTab = writer(X,N,Ty,DepTab),
    NewDAG = write_deps(N,Ty,NxtWriter,NxtReaders,DAG),
    {NewDepTab, NewDAG}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : write_deps
%% Argument    : Instr      - Current instr 
%%               Ty         - Type of current instr
%%               NxtWriter  - The node that is the next writer of the ragister
%%                            that Instr defines.
%%               NxtReaders - The nodes that are subsequent readers of the
%%                            register that N defines.
%%               DAG        - The dependence graph
%% Returns     : Calls raw_deps that finally returns a new DAG with the new
%%               dependence arcs added.
%% Description : If a next writer exists a dependence arc for this node is 
%%               added, and after this raw_deps is called to compute the 
%%               arcs for read-after-write dependencies. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_deps(Instr,Ty,NxtWriter,NxtReaders,DAG) ->
    DAG1 = case NxtWriter of
	       none ->
		   DAG;
	       {Instr,_} ->
		   DAG;
	       {Wr,WrTy} ->
		   dep_arc(Instr,
			   hipe_target_machine:waw_latency(Ty,WrTy),
			   Wr, DAG)
	   end,
    raw_deps(Instr,Ty,NxtReaders,DAG1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : raw_deps
%% Argument    : Instr   - current instr
%%               Type    - type of instr
%%               Readers - subsequent readers 
%%               DAG     - dependence graph
%% Returns     : DAG - A new DAG with read-after-write dependencies added
%% Description : Updates the DAG with the dependence-arcs from Instr to the
%%               subsequent readers, with the appropriate latencies.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
raw_deps(_Instr,_Type,[],DAG) -> DAG;
raw_deps(Instr,Ty,[{Rd,RdTy}|Xs],DAG) ->
    raw_deps(Instr,Ty,Xs,
	     dep_arc(Instr,hipe_target_machine:raw_latency(Ty,RdTy),
		     Rd,DAG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_read_deps
%% Argument    : Uses      - The registers that node N uses.
%%               N         - Index of the current node.
%%               Ty        - Type of current node.
%%               DepTab    - Dependence table
%%               DAG       - Dependence graph
%% Returns     : {DepTab, DAG} - with updated values.
%% Description : Adds the read dependencies from node N to subsequent ones, 
%%               according to the registers that N uses. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_read_deps([],_N,_Ty,DepTab,DAG) -> {DepTab,DAG};
add_read_deps([U|Us],N,Ty,DepTab,DAG) ->
    {NewDepTab,NewDAG} = add_read_dep(U,N,Ty,DepTab,DAG),
    add_read_deps(Us,N,Ty,NewDepTab,NewDAG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_read_dep
%% Argument    : X      - Used register
%%               N      - Index of checked instr
%%               Ty     - Type of checked instr
%%               DepTab - Hashtable with {next-writer, next-readers}
%%               DAG    - Dependence graph
%% Returns     : {DepTab, DAG} - with updated values
%% Description : Looks up what the next-writer/next-readers are, and adjusts
%%               the table with current node as new reader. Finally 
%%               read-dependencies are added to the DAG.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_read_dep(X,N,Ty,DepTab,DAG) ->
    {NxtWriter,_NxtReaders} = lookup(X,DepTab),
    NewDepTab = reader(X,N,Ty,DepTab),
    NewDAG = read_deps(N,Ty,NxtWriter,DAG),
    {NewDepTab, NewDAG}.

% If NxtWriter is 'none', then this var is not written subsequently
% Add WAR from Instr to NxtWriter (if it exists)
% *** UNFINISHED ***
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : read_deps
%% Argument    : N      - Index of current node
%%               Ty     - Type of current node
%%               Writer - tuple {NextWriter, WrType} where NextWriter is the
%%                        subsequent instr that writes this register next time,
%%                        and WrType is the type of that instr.
%%               DAG    - The dependence graph
%% Returns     : DAG 
%% Description : Returns a new DAG if a next-writer exists, otherwise the old 
%%               DAG is returned.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_deps(_Instr,_Ty,none,DAG) ->
    DAG;
read_deps(_Instr,_Ty,{_Instr,_},DAG) ->
    DAG;
read_deps(Instr,Ty,{NxtWr,NxtWrTy},DAG) ->
    dep_arc(Instr,hipe_target_machine:war_latency(Ty,NxtWrTy),NxtWr,
	    DAG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : empty_deptab
%% Description : Creates an empty dependence table (hash-table)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_deptab() ->
    gb_trees:empty().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : lookup
%% Argument    : X      - key (register)
%%               DepTab - dependence table
%% Returns     : {NextWriter, NextReaders}
%% Description : Returns next writer and a list of following readers on 
%%               register X.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup(X, DepTab) ->
    case gb_trees:lookup(X, DepTab) of
	none ->
	    {none, []};
	{value, {W, Rs} = Val} ->
	    Val
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : writer
%% Argument    : X      - key (register)
%%               N      - index of writer
%%               Ty     - type of writer
%%               DepTab - dependence table to be updated
%% Returns     : DepTab - new dependence table
%% Description : Sets N tobe next writer on X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
writer(X, N, Ty, DepTab) ->
    gb_trees:enter(X, {{N, Ty}, []}, DepTab).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : reader
%% Argument    : X      - key (register)
%%               N      - index of reader
%%               Ty     - type of reader
%%               DepTab - dependence table to be updated
%% Returns     : DepTab - new dependence table
%% Description : Adds N to the dependence table as a reader.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reader(X,N,Ty,DepTab) ->
    {W,Rs} = lookup(X,DepTab),
    gb_trees:enter(X,{W,[{N,Ty}|Rs]},DepTab).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The following version of md/2 separates heap- and stack operations,
%% which allows for greater reordering.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : md
%% Argument    : IxBB - indexed block
%%               DAG  - dependence graph
%% Returns     : DAG  - new dependence graph
%% Description : Adds arcs for load/store dependencies to the DAG.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
md(IxBB, DAG) ->
    md(IxBB,empty_md_state(),DAG).

md([],_,DAG) -> DAG;
md([{N,I}|Is],St,DAG) ->
    case md_type(I) of
	other ->
	    md(Is,St,DAG);
	{st,T} ->
	    { WAW_nodes, WAR_nodes, NewSt } = st_overlap(N,T,St),
	    md(Is,NewSt, 
		md_war_deps(WAR_nodes,N,md_waw_deps(WAW_nodes,N,DAG)));
	{ld,T} ->
	    { RAW_nodes, NewSt } = ld_overlap(N,T,St),
	    md(Is,NewSt,
		md_raw_deps(RAW_nodes,N,DAG))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : md_war_deps
%% Argument    : WAR_nodes - write-after-read nodes depending on N
%%               N         - index of current instr
%%               DAG       - dependence graph
%% Returns     : DAG - updated DAG
%% Description : Adds arcs for write-after-read dependencies for N
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
md_war_deps([],_,DAG) -> DAG;
md_war_deps([M|Ms],N,DAG) ->
    md_war_deps(Ms,N,dep_arc(M,hipe_target_machine:m_war_latency(),N,DAG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : md_waw_deps
%% Argument    : WAW_nodes - write-after-write nodes depending on N
%%               N         - index of current instr
%%               DAG       - dependence graph
%% Returns     : DAG - updated DAG
%% Description : Adds arcs for write-after-write dependencies for N
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
md_waw_deps([],_,DAG) -> DAG;
md_waw_deps([M|Ms],N,DAG) ->
    md_waw_deps(Ms,N,dep_arc(M,hipe_target_machine:m_waw_latency(),N,DAG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : md_raw_deps
%% Argument    : RAW_nodes - read-after-write nodes depending on N
%%               N         - index of current instr
%%               DAG       - dependence graph
%% Returns     : DAG - updated DAG
%% Description : Adds arcs for read-after-write dependencies for N
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
md_raw_deps([],_,DAG) -> DAG;
md_raw_deps([M|Ms],N,DAG) ->
    md_raw_deps(Ms,N,dep_arc(M,hipe_target_machine:m_raw_latency(),N,DAG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : empty_md_state
%% Description : Returns an empty memorydependence state, eg. 4 lists 
%%               representing {StackStores, HeapStores, StackLoads, HeapLoads}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_md_state() -> {[], [], [], []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : md_type
%% Argument    : I - instr
%% Description : Maps the instr-type to a simplified type, telling if it's
%%               store/load resp. heap or stack.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
md_type(I) ->
    case I of
	#load{} ->
	    Sp = hipe_sparc_registers:stack_pointer(),
	    Src = hipe_sparc:load_src(I),
	    N = hipe_sparc:reg_nr(Src),
	    Off = hipe_sparc:load_off(I),
	    if
		N =:= Sp -> % operation on stack
		    {ld,{sp,Off}};
		true ->
		    {ld,{hp,Src,Off}}
	    end;
	#store{} ->
	    Sp = hipe_sparc_registers:stack_pointer(),
	    Dst = hipe_sparc:store_dest(I),
	    N = hipe_sparc:reg_nr(Dst),
	    Off = hipe_sparc:store_off(I),
	    if
		N =:= Sp ->
		    {st,{sp,Off}};
		true ->
		    {st,{hp,Dst,Off}}
	    end;
	_ ->
	    other
    end.

%% Given a memory operation and a 'memory op state',
%% overlap(N,MemOp,State) returns { Preceding_Dependent_Ops, NewState }.
%%  which are either a tuple { WAW_deps, WAR_deps } or a list RAW_deps.
%%
%% NOTES:
%%  Note that Erlang's semantics ("heap stores never overwrite existing data")
%% means we can be quite free in reordering stores to the heap.
%%  Ld/St to the stack are simply handled by their offsets; since we do not
%% rename the stack pointer, this is sufficient.
%%  *** We assume all memory ops have uniform size = 4 ***
%%
%% NOTES:
%%  The method mentioned above has now been changed because the assumption that
%%  "heap stores never overwrite existing data" caused a bug when the
%%   process-pointer was treated the same way as the heap. We were also told 
%%   that the semantics can possibly change in the future, so it would be more 
%%   safe to treat the heap store/loads as the stack.
%%   A future improvement can be to do an alias analysis to give more freedom
%%   in reordering stuff...
%%
%% Alias state:
%%   { [StackOp], [HeapOp], [StackOp], [HeapOp] }
%% where StackOp = {InstrID, Offset}
%%       HeapOp = {InstrID, Reg, Offset}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : st_overlap
%% Argument    : N     - Index of current node
%%               Type  - {sp,Off} or {hp,Dst,Off}, store on stack or heap
%%               State - { [StackStrs], [HeapStrs], [StackLds], [HeapLds] }
%%                       where StackStrs/StackLds = {InstrID, Offset}
%%                       and    HeapStrs/HeapLds  = {InstrID, Reg, Offset}
%% Returns     : { DepStrs, DepLds, State } -
%%                   where DepStrs/DepLds = [NodeId]
%%                   and State is the new state
%% Description : Adds dependencies for overlapping stores.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
st_overlap(N, {sp, Off}, {St_Sp, St_Hp, Ld_Sp, Ld_Hp}) ->
    {DepSt, IndepSt_Sp} = st_sp_dep(St_Sp, Off),
    {DepLd, IndepLd_Sp} = ld_sp_dep(Ld_Sp, Off),
    {DepSt, DepLd, {[{N, Off}|IndepSt_Sp], St_Hp, IndepLd_Sp, Ld_Hp}};
st_overlap(N, {hp, Dst, Off}, {St_Sp, St_Hp, Ld_Sp, Ld_Hp}) ->
    DstOff = {Dst, Off},
    {DepSt,_IndepSt_Hp} = st_hp_dep(St_Hp, DstOff),
    {DepLd, IndepLd_Hp} = ld_hp_dep(Ld_Hp, DstOff),
    {DepSt, DepLd, {St_Sp, [{N, Dst, Off}|St_Hp], Ld_Sp, IndepLd_Hp}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : ld_overlap
%% Argument    : N     - Index of current node
%%               Type  - {sp,Off} or {hp,Dst,Off}, store on stack or heap
%%               State - { [StackStrs], [HeapStrs], [StackLds], [HeapLds] }
%%                       where StackStrs/StackLds = {InstrID, Offset}
%%                       and    HeapStrs/HeapLds  = {InstrID, Reg, Offset}
%% Returns     : { DepStrs, State } 
%% Description : Adds dependencies for overlapping laods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ld_overlap(N, {sp, Off}, {St_Sp, St_Hp, Ld_Sp, Ld_Hp}) ->
    DepSt = sp_dep_only(St_Sp, Off),
    {DepSt, {St_Sp, St_Hp, [{N, Off}|Ld_Sp], Ld_Hp}};
ld_overlap(N, {hp, Src, Off}, {St_Sp, St_Hp, Ld_Sp, Ld_Hp}) ->
    DepSt = hp_dep_only(St_Hp, Src, Off),
    {DepSt, {St_Sp, St_Hp, Ld_Sp, [{N, Src, Off}|Ld_Hp]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : st_sp_dep
%% Description : Adds dependencies that are depending on a stack store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
st_sp_dep(Stores, Off) ->
    sp_dep(Stores, Off, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : ld_sp_dep
%% Description : Adds dependencies that are depending on a stack load
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ld_sp_dep(Loads, Off) ->
    sp_dep(Loads, Off, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : st_hp_dep
%% Description : Adds dependencies that are depending on a heap store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
st_hp_dep(Stores, {_Reg, _Off} = RegOff) ->
    hp_dep(Stores, RegOff, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : ld_hp_dep
%% Description : Adds dependencies that are depending on a heap load
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ld_hp_dep(Loads, {_Reg, _Off} = RegOff) ->
    hp_dep(Loads, RegOff, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : sp_dep
%% Description : Returns {Dependent, Independent} which are lists of nodes
%%               that depends or not on a stack load/store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sp_dep([], _Off, Dep, Indep) -> {Dep, Indep};
sp_dep([{N,Off}|Xs], Off, Dep, Indep) ->
    sp_dep(Xs, Off, [N|Dep], Indep);
sp_dep([X|Xs], Off, Dep, Indep) ->
    sp_dep(Xs, Off, Dep, [X|Indep]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : hp_dep
%% Description : Returns {Dependent, Independent} which are lists of nodes
%%               that depends or not on a heap load/store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hp_dep([], {_Reg,_Off}, Dep, Indep) -> {Dep,Indep};
hp_dep([{N,Reg,Off1}|Xs], {Reg,Off}, Dep, Indep) when Off1 =/= Off ->
    hp_dep(Xs, {Reg,Off}, Dep, [{N,Reg,Off1}|Indep]);
hp_dep([{N,_,_}|Xs], {Reg,Off}, Dep, Indep) ->
    hp_dep(Xs, {Reg,Off}, [N|Dep], Indep).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : sp_dep_only
%% Description : Returns a list of nodes that are depending on a stack store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sp_dep_only(Stores, Off) ->
    [N || {N,Off0} <- Stores, Off =:= Off0].

%% Dependences from heap stores to heap loads.
%% *** UNFINISHED ***
%% - but works
%% This is somewhat subtle:
%% - a heap load can only bypass a heap store if we KNOW it won't
%%   load the stored value
%% - unfortunately, we do not know the relationships between registers
%%   at this point, so we can't say that store(p+4) is independent of
%%   load(q+0).
%%    (OR CAN WE? A bit closer reasoning might show that it's possible?)
%% - We can ONLY say that st(p+c) and ld(p+c') are independent when c /= c'
%%
%% (As said before, it might be possible to lighten this restriction?)

hp_dep_only([], _Reg, _Off) -> [];
hp_dep_only([{_N,Reg,Off_1}|Xs], Reg, Off) when Off_1 =/= Off ->
    hp_dep_only(Xs, Reg, Off);
hp_dep_only([{N,_,_}|Xs], Reg, Off) ->
    [N|hp_dep_only(Xs, Reg, Off)].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control dependences:
%% - add dependences so that
%%   * branches are performed in order
%%   * unsafe operations are 'fenced in' by surrounding branches
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cd
%% Argument    : IxBB - indexed block
%%               DAG  - dependence graph
%% Returns     : DAG - new dependence graph
%% Description : Adds conditional dependencies to the DAG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd(IxBB,DAG) ->
    cd(IxBB, DAG, none, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cd
%% Argument    : IxBB - indexed block
%%               DAG  - dependence graph
%%               PrevBr - previous branch
%%               PrevUnsafe - previous unsafe instr (mem-op)
%%               PrevOthers - previous other instrs, used to "fix" preceeding
%%                            instrs so they don't bypass a branch.
%% Returns     : DAG - new dependence graph
%% Description : Adds conditional dependencies  to the graph.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd([], DAG, _PrevBr, _PrevUnsafe, _PrevOthers) ->
    DAG;
cd([{N,I}|Xs], DAG, PrevBr, PrevUnsafe, PrevOthers) ->
    case cd_type(I) of
	{branch,Ty} ->
	    DAG1   = cd_branch_to_other_deps(N, PrevOthers, DAG),
	    NewDAG = cd_branch_deps(PrevBr, PrevUnsafe, N, Ty, DAG1),
	    cd(Xs,NewDAG,{N,Ty},[],[]);
	{unsafe,Ty} ->
	    NewDAG = cd_unsafe_deps(PrevBr,N,Ty,DAG),
	    cd(Xs, NewDAG, PrevBr, [{N,Ty}|PrevUnsafe], PrevOthers);
	{other,_Ty} ->
	    cd(Xs, DAG, PrevBr, PrevUnsafe, [N|PrevOthers])
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cd_branch_to_other_deps
%% Argument    : N   - index of branch
%%               Ms  - list of indexes of "others" preceeding instrs
%%               DAG - dependence graph
%% Returns     : DAG - new graph
%% Description : Makes preceeding instrs fixed so they don't bypass a branch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd_branch_to_other_deps(_, [], DAG) ->
    DAG;
cd_branch_to_other_deps(N, [M | Ms], DAG) ->
    cd_branch_to_other_deps(N, Ms, dep_arc(M, zero_latency(), N, DAG)).

%% Is the operation a branch, an unspeculable op or something else?

%% Returns
%%   {branch,BranchType}
%%   {unsafe,OpType}
%%   {other,OpType}

%% *** UNFINISHED ***
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cd_type
%% Argument    : I - instr
%% Description : Maps instrs to a simpler type.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd_type(I) ->
   case I of
	#goto{} ->
	    {branch,uncond};
	#br{} ->
	    {branch,'cond'};
	#b{} ->
	    {branch,'cond'};
	#call_link{} ->
	    {branch,call};
	#jmp_link{} ->
	   {branch,call};
	#jmp{} ->
	    {branch,call};
	#load{} ->
	    {unsafe,load};
	#store{} ->
	    {unsafe,load};
	T ->
	    {other,T}
   end.

%% add dependences to keep order of branches + unspeculable ops:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cd_branch_deps
%% Argument    : PrevBr     - preceeding branch
%%               PrevUnsafe - preceeding unsafe ops, eg, mem-ops
%%               N          - current id.
%%               Ty         - type of current instr
%%               DAG        - dependence graph 
%% Returns     : DAG - new DAG
%% Description : Adds arcs between branches and calls deps_to_unsafe that adds
%%               arcs between branches and unsafe ops.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd_branch_deps(PrevBr, PrevUnsafe, N, Ty, DAG) ->
    DAG1 = case PrevBr of
	       none ->
		   DAG;
	       {Br,BrTy} ->
		   dep_arc(Br,
			   hipe_target_machine:br_br_latency(BrTy,Ty),
			   N, DAG)
	   end,
    deps_to_unsafe(PrevUnsafe, N, Ty, DAG1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : deps_to_unsafe
%% Description : Adds dependencies between unsafe's and branches
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deps_to_unsafe([], _, _, DAG) -> DAG;
deps_to_unsafe([{M,UTy}|Us], N, Ty, DAG) ->
    deps_to_unsafe(Us,N,Ty,
		   dep_arc(M, hipe_target_machine:unsafe_to_br_latency(UTy,Ty),
			   N, DAG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cd_unsafe_deps
%% Description : Adds dependencies between branches and unsafe's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd_unsafe_deps(none, _, _, DAG) ->
    DAG;
cd_unsafe_deps({Br,BrTy}, N, Ty, DAG) ->
    dep_arc(Br, hipe_target_machine:br_to_unsafe_latency(BrTy, Ty), N, DAG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : def_use
%% Argument    : Instr 
%% Description : Returns the registers that Instr defines resp. uses as 2 lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
def_use(Instr) ->
    {hipe_sparc:defines(Instr), hipe_sparc:uses(Instr)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : move_or_alu
%% Description : True if the instruction is a move or an alu; false otherwise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_or_alu(#move{}) -> true;
move_or_alu(#alu{}) -> true;
move_or_alu(_) -> false.

%% Debugging stuff below %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(debug1).
debug1_stuff(Blk, DAG, IxBlk, Sch, X) ->
    io:format("Blk: ~p~n",[Blk]),
    io:format("DAG: ~n~p~n~p",[DAG,IxBlk]),
    io:format("~n"),
    print_instrs(IxBlk),
    print_sch(Sch, IxBlk),
    print_instrs2(X).

print_instrs([]) ->
    io:format("~n");
print_instrs([{N,Instr} | Instrs]) ->
    io:format("(~p): ",[N]),
    hipe_sparc_pp:pp_instr(Instr),
    io:format("~p~n",[element(1,Instr)]),
    print_instrs(Instrs).

print_instrs2([]) ->
    io:format("~n");
print_instrs2([Instr | Instrs]) ->
    hipe_sparc_pp:pp_instr(Instr),
    print_instrs2(Instrs).

print_sch([],_) -> io:format("~n");
print_sch([{{cycle,Cycle},{node,I}} | Rest], IxBlk) ->
    io:format("{C~p, N~p} ",[Cycle,I]),
    print_node(I, IxBlk),
    print_sch(Rest, IxBlk).

print_node(_, []) ->
    io:format("~n");
print_node(I, [{I, Instr} | _]) ->
    hipe_sparc_pp:pp_instr(Instr);
print_node(I, [_ | IxBlk]) ->
    print_node(I, IxBlk).
-else.
debug1_stuff(_Blk, _DAG, _IxBlk, _Sch, _X) ->
    ok.
-endif.
