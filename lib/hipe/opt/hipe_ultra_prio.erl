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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%	      PRIORITY HANDLING AND PRIORITY CALCULATION
%%
%% Handling of ready nodes and priorities.
%% Priorities are mainly from the critical path. More priorities are added.
%%  * One version is adding priorities just depending on the instr, so
%%    for example loads get higher priority than stores, and ordered
%%    after reg's and offset for better cache performance.
%%  * The other version gives higher priority to a node that adds more new
%%    nodes to the ready list. This one is maybe not so effectively
%%    implemented, but was added too late for smarter solutions.
%% One version is commented away

-module(hipe_ultra_prio).
-export([init_ready/2,
	 init_instr_prio/2,
	 %% initial_ready_set/4,
	 next_ready/7,
	 add_ready_nodes/2,
	 insert_node/3
	]).

-include("../sparc/hipe_sparc.hrl").

% At first, only nodes with no predecessors are selected.
% - if R is empty, there is an error (unless BB itself is empty)

%% Arguments : Size  - size of ready-array
%%             Preds - array with number of predecessors for each node
%% Returns   : An array with list of ready-nodes for each cycle.

init_ready(Size, Preds) ->
    P = hipe_vectors:size(Preds),
    Ready = hipe_vectors:new(Size, []),
    R = initial_ready_set(1, P, Preds, []),
    hipe_vectors:set(Ready, 0, R).

init_instr_prio(N, DAG) ->
    critical_path(N, DAG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : initial_ready_set
%% Argument    : M     - current node-index
%%               N     - where to stop
%%               Preds - array with number of predecessors for each node
%%               Ready - list with ready-nodes
%% Returns     : Ready - list with ready-nodes
%% Description : Finds all nodes with no predecessors and adds them to ready.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_ready_set(M, N, Preds, Ready) ->
    if
	M > N ->
	    Ready;
	true ->
	    case hipe_vectors:get(Preds, M-1) of
		0 ->
		    initial_ready_set(M+1, N, Preds, [M|Ready]);
		V when is_integer(V), V > 0 ->
		    initial_ready_set(M+1, N, Preds, Ready)
	    end
    end.

%% The following handles the nodes ready to schedule:
%% 1. select the ready queue of given cycle
%% 2. if queue empty, return none
%% 3. otherwise, remove entry with highest priority
%%    and return {next,Highest_Prio,NewReady}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : next_ready
%% Argument    : C     - current cycle
%%               Ready - array with ready nodes
%%               Prio  - array with cpath-priorities for all nodes
%%               Nodes - indexed list [{N, Instr}]
%% Returns     : none / {next,Highest_Prio,NewReady}
%% Description :  1. select the ready queue of given cycle
%%                2. if queue empty, return none
%%                3. otherwise, remove entry with highest priority
%%                   and return {next,Highest_Prio,NewReady} where Highest_Prio
%%                   = Id of instr and NewReady = updated ready-array.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_ready(C, Ready, Prio, Nodes, DAG, Preds, Earl) ->
    Curr = hipe_vectors:get(Ready, C-1),
    case Curr of
	[] -> 
	    none;
	Instrs ->
	    {BestI,RestIs} = 
		get_best_instr(Instrs, Prio, Nodes, DAG, Preds, Earl, C),
	    {next,BestI,hipe_vectors:set(Ready,C-1,RestIs)}
    end.

% next_ready(C,Ready,Prio,Nodes) ->
%     Curr = hipe_vectors:get(Ready,C-1),
%     case Curr of
% 	[] ->   
% 	    none;
% 	Instrs ->
% 	    {BestInstr,RestInstrs} = get_best_instr(Instrs, Prio, Nodes),
% 	    {next,BestInstr,hipe_vectors:set(Ready,C-1,RestInstrs)}
%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : get_best_instr
%% Argument    : Instrs - list of node-id's
%%               Prio   - array with cpath-priorities for the nodes
%%               Nodes  - indexed list [{Id, Instr}]
%% Returns     : {BestSoFar, Rest} - Id of best instr and the rest of id's
%% Description : Returns the id of the instr that is the best choice.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_best_instr([Instr|Instrs], Prio, Nodes, DAG, Preds, Earl, C) ->
    get_best_instr(Instrs, [], Instr, Prio, Nodes, DAG, Preds, Earl, C).

get_best_instr([], Rest, BestSoFar, _Prio, _Nodes, _DAG, _Preds, _Earl, _C) -> 
    {BestSoFar, Rest};
get_best_instr([Instr|Instrs], PassedInstrs, BestSoFar, Prio, Nodes,
	       DAG, Preds, Earl, C) ->
    case better(Instr, BestSoFar, Prio, Nodes, DAG, Preds, Earl, C) of
        true ->
	    get_best_instr(Instrs, [BestSoFar|PassedInstrs],
			   Instr, Prio, Nodes, DAG, Preds, Earl, C);
	false -> 
	    get_best_instr(Instrs, [Instr|PassedInstrs], BestSoFar, Prio, 
			   Nodes, DAG, Preds, Earl, C)
    end.

% get_best_instr([Instr|Instrs], Prio, Nodes) ->
%     get_best_instr(Instrs, [], Instr, Prio, Nodes).

% get_best_instr([], Rest, BestSoFar, Prio, Nodes) -> {BestSoFar, Rest};
% get_best_instr([Instr|Instrs], PassedInstrs, BestSoFar, Prio, Nodes) ->
%     case better(Instr, BestSoFar, Prio, Nodes) of
%         true ->
% 	    get_best_instr(Instrs, [BestSoFar|PassedInstrs], 
% 			   Instr, Prio, Nodes);
% 	false -> 
% 	    get_best_instr(Instrs, [Instr|PassedInstrs],BestSoFar, Prio, Nodes)
%     end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : better
%% Argument    : Instr1 - Id of instr 1
%%               Instr2 - Id of instr 2
%%               Prio   - array with cpath-priorities for the nodes
%%               Nodes  - indexed list [{Id, Instr}]
%% Returns     : true if Instr1 has higher priority than Instr2
%% Description : Checks if Instr1 is a better choice than Instr2 for scheduling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

better(Instr1, Instr2, Prio, Nodes, DAG, Preds, Earl, C) ->
    better_hlp(priority(Instr1, Prio, Nodes, DAG, Preds, Earl, C), 
	       priority(Instr2, Prio, Nodes, DAG, Preds, Earl, C)).

better_hlp([], []) -> false;
better_hlp([], [_|_]) -> false;
better_hlp([_|_], []) -> true;
better_hlp([X|Xs], [Y|Ys]) -> (X > Y) or ((X =:= Y) and better_hlp(Xs,Ys)).

%%
%% Returns the instr corresponding to id
%%
get_instr(InstrId, [{InstrId,Instr}|_]) -> Instr;
get_instr(InstrId, [_|Xs]) -> get_instr(InstrId, Xs).
				      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : priority
%% Argument    : InstrId - Id
%%               Prio    - array with cpath-priorities for the nodes
%%               Nodes   - indexed list [{Id, Instr}]
%% Returns     : PrioList - list of priorities [MostSignificant, LessSign, ...]
%% Description : Returns a list of priorities where the first element is the
%%               cpath-priority and the rest are added depending on what kind
%%               of instr it is. Used to order loads/stores sequentially and
%%               there is possibility to add whatever stuff...  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

priority(InstrId, Prio, Nodes, DAG, Preds, Earl, C) ->
    {ReadyNodes,_,_,_} = hipe_schedule:delete_node(C,InstrId,DAG,Preds,Earl),
    Instr = get_instr(InstrId, Nodes),
    Prio1 = hipe_vectors:get(Prio, InstrId-1),
    Prio2 = length(ReadyNodes),
    PrioRest =
	case Instr of
	 #load_atom{} ->
	    [3];
	 #move{} ->
	    [3];
	 #load{} -> 
  	    Src = hipe_sparc:load_src(Instr),
  	    Off = hipe_sparc:load_off(Instr),
	    case hipe_sparc:is_reg(Off) of
		false -> [3, 
			  -(hipe_sparc:reg_nr(Src)), 
			  -(hipe_sparc:imm_value(Off))];
		true -> [1]
	    end;
	 #store{} -> 
	     Src = hipe_sparc:store_dest(Instr),
	     Off = hipe_sparc:store_off(Instr),
	     case hipe_sparc:is_reg(Off) of
		 false -> [2, 
			   -(hipe_sparc:reg_nr(Src)), 
			   -(hipe_sparc:imm_value(Off))];
		 true -> [1]
	     end;
	 _ -> [0]
	end,
    [Prio1,Prio2|PrioRest].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : add_ready_nodes
%% Argument    : Nodes - list of [{Cycle,Id}]
%%               Ready - array of ready nodes for all cycles
%% Returns     : NewReady - updated ready-array
%% Description : Gets a list of instrs and adds them to the ready-array
%%               to the corresponding cycle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_ready_nodes([], Ready) -> Ready;
add_ready_nodes([{C,I}|Xs], Ready) ->
    add_ready_nodes(Xs, insert_node(C, I, Ready)).

insert_node(C, I, Ready) ->
    Old = hipe_vectors:get(Ready, C-1),
    hipe_vectors:set(Ready, C-1, [I|Old]).

%%
%% Computes the latency for the "most expensive" way through the graph
%% for all nodes.  Returns an array of priorities for all nodes.
%%
critical_path(N, DAG) ->
    critical_path(1, N, DAG, hipe_vectors:new(N, -1)).

critical_path(M, N, DAG, Prio) ->
    if
	M > N ->
	    Prio;
	true ->
	    critical_path(M+1, N, DAG, cpath(M, DAG, Prio))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function    : cpath
%% Argument    : M    - current node id
%%               DAG  - the dependence graph
%%               Prio - array of priorities for all nodes
%% Returns     : Prio - updated prio array
%% Description : If node has prio -1, it has not been visited
%%                - otherwise, compute priority as max of priorities of 
%%                  successors (+ latency)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cpath(M, DAG, Prio) ->
    InitPrio = hipe_vectors:get(Prio, M-1),
    if
	InitPrio =:= -1 ->
	    cpath_node(M, DAG, Prio);
	true ->
	    Prio
    end.

cpath_node(N, DAG, Prio) ->
    SuccL = dag_succ(DAG, N),
    {Max, NewPrio} = cpath_succ(SuccL, DAG, Prio),
    hipe_vectors:set(NewPrio, N-1, Max).

cpath_succ(SuccL, DAG, Prio) ->
    cpath_succ(SuccL, DAG, Prio, 0).

%% performs an unnecessary lookup of priority of Succ, but that might
%% not be such a big deal

cpath_succ([], _DAG, Prio, NodePrio) -> {NodePrio,Prio};
cpath_succ([{Lat,Succ}|Xs], DAG, Prio, NodePrio) ->
    NewPrio = cpath(Succ, DAG, Prio),
    NewNodePrio = erlang:max(hipe_vectors:get(NewPrio, Succ - 1) + Lat, NodePrio),
    cpath_succ(Xs, DAG, NewPrio, NewNodePrio).

dag_succ(DAG, N) when is_integer(N) ->
    hipe_vectors:get(DAG, N-1).

