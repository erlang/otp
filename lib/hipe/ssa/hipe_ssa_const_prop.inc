%% -*- Erlang -*-
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
%%-----------------------------------------------------------------------------
%% File        : hipe_ssa_const_prop.inc
%% Author      : Kostis Sagonas <kostis@it.uu.se>
%% Description : Supporting routines for sparse conditional constant
%%		 propagation on SSA form.
%%
%% Created     : 21 June 2004 by Kostis Sagonas <kostis@it.uu.se>
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Procedure : propagate/1
%% Purpose   : Perform sparse conditional constant propagation on a
%%             control flow graph
%% Arguments : CFG  - The cfg to work on
%% Returns   : A new cfg.
%%-----------------------------------------------------------------------------

-spec propagate(#cfg{}) -> #cfg{}.

propagate(CFG) ->
  Environment    = create_env(CFG),
  StartEdge      = {?CFG:start_label(CFG), ?CFG:start_label(CFG)},
  NewEnvironment = scc([StartEdge], [], Environment),
  NewCFG         = update_cfg(NewEnvironment),
  NewCFG.

%%-----------------------------------------------------------------------------
%% Procedure : visit_expressions/2 & visit_expressions/4
%% Purpose   : visit each instruction in a list of instructions.
%% Arguments : Instructions - the list of instructions to visit
%%             Environment  - have a guess.
%%             FlowWork     - list of destination part of flowgraph edges 
%%                            from the visited instructions
%%             SSAWork      - resulting ssa-edges from visited instrs.
%% Returns   : {FlowWorkList, SSAWorkList, Environment} 
%%-----------------------------------------------------------------------------

visit_expressions(Instructions, Environment) ->
  visit_expressions(Instructions, Environment, [], []).

visit_expressions([], Environment, FlowWork, SSAWork) ->
  {FlowWork, SSAWork, Environment};
visit_expressions([Inst | Insts], Environment, FlowWork, SSAWork) ->
  {MoreFlowWork, MoreSSAWork, Environment1}
    = visit_expression(Inst, Environment),
  FlowWork1 = MoreFlowWork ++ FlowWork,
  SSAWork1  = MoreSSAWork  ++ SSAWork,
  visit_expressions(Insts, Environment1, FlowWork1, SSAWork1).

%%-----------------------------------------------------------------------------
%% The environment record: Shared between incarnations of SCCP.
%%-----------------------------------------------------------------------------

-record(env, {cfg                                 :: #cfg{},
	      executable_flags = gb_sets:empty()  :: gb_sets:set(),
	      handled_blocks   = gb_sets:empty()  :: gb_sets:set(),
	      lattice_values   = gb_trees:empty() :: gb_trees:tree(),
	      ssa_edges        = gb_trees:empty() :: gb_trees:tree()
	     }).

create_env(CFG) ->
  #env{cfg              = CFG,
       executable_flags = gb_sets:empty(),
       handled_blocks   = gb_sets:empty(),
       lattice_values   = initialize_lattice(CFG),
       ssa_edges        = initialize_ssa_edges(CFG)
      }.

env__cfg(#env{cfg=CFG}) -> CFG.
env__executable_flags(#env{executable_flags=Flags}) -> Flags.
env__lattice_values(#env{lattice_values=Values}) -> Values.
env__ssa_edges(#env{ssa_edges=Edges}) -> Edges.

%%-----------------------------------------------------------------------------
%% Procedure : initialize_lattice/1
%% Purpose   : Compute the initial value-lattice for the CFG
%% Arguments : CFG a control flow graph
%% Returns   : a value-latice (gb_tree)
%%-----------------------------------------------------------------------------

initialize_lattice(CFG) ->
  Lattice    = gb_trees:empty(),
  Parameters = ?CFG:params(CFG),
  Inserter   = fun(Parameter, Tree) ->
                   gb_trees:insert(Parameter, bottom, Tree)
	       end,
  lists:foldl(Inserter, Lattice, Parameters).

%%-----------------------------------------------------------------------------
%% Procedure : initialize_ssa_edges/1
%% Purpose   : Compute the SSA edges in the CFG. SSA edges are used to map
%%             the definition of a value to its uses.
%% Arguments : CFG - the cfg
%% Returns   : A gb_tree of values (variables & registers) to 
%%             lists of {Node, Instruction} pairs.
%%-----------------------------------------------------------------------------

initialize_ssa_edges(CFG) ->
  IterateNodes =
    fun(Node, Tree1) ->
        IterateInstructions =
          fun(Instruction, Tree2) ->
              IterateArguments =
                fun(Argument, Tree3) ->
                    Data    = gb_trees:lookup(Argument, Tree3),
                    NewEdge = {Node, Instruction},
                    case Data of
                      none ->
                        %% insert assumes key is not present
                        gb_trees:insert(Argument, [NewEdge], Tree3);
                      {value, EdgeList} ->
                        %% update assumes key is present
                        gb_trees:update(Argument, [NewEdge|EdgeList], Tree3)
                    end
                end,
              Arguments = ?CODE:uses(Instruction),
              lists:foldl(IterateArguments, Tree2, Arguments)
          end,
        Instructions = hipe_bb:code(?CFG:bb(CFG, Node)),
        lists:foldl(IterateInstructions, Tree1, Instructions)
    end,
  NodeList = ?CFG:labels(CFG),
  lists:foldl(IterateNodes, gb_trees:empty(), NodeList).

%%-----------------------------------------------------------------------------
%% Procedure : scc/3
%% Purpose   : Do the symbolic execution of a cfg and compute the resulting 
%%             value-lattice, and reachability information (Environment).
%%             This is the main loop that does a fixpoint computation of the 
%%             lattice-values for each variable and register.
%% Arguments : FlowWorkList - work list of control-flow edges
%%             SSAWorkList  - work list of ssa-edges
%%             Environment  - the environment that have been computed so far.
%% Returns   : The environment after execution 
%%-----------------------------------------------------------------------------

scc([], [], Environment) ->
  Environment;
%% Take an element from the FlowWorkList and process it
scc([{Source,Destination} | FlowWorkList], SSAWorkList, Environment) ->
  case executable({Source, Destination}, Environment) of
    true ->
      scc(FlowWorkList, SSAWorkList, Environment);
    false ->
      Environment1 = mark_as_executable({Source,Destination}, Environment),
      Code         = extract_code(Destination, Environment),
      {Environment2, Code1, ExtraSSA} =
        visit_phi_nodes(Code, Destination, Environment1, []),
      case handled(Destination, Environment2) of
        true ->
          scc(FlowWorkList, ExtraSSA ++ SSAWorkList, Environment2);
        false ->
          {MoreFlowDests, MoreSSAWork, Environment3} = 
            visit_expressions(Code1, Environment2),
          MoreFlowWork  = [{Destination, Node} || Node <- MoreFlowDests],
          FlowWorkList1 = MoreFlowWork ++ FlowWorkList,
          SSAWorkList1  = ExtraSSA ++ MoreSSAWork ++ SSAWorkList,
	  Environment4  = mark_as_handled(Destination, Environment3),
          scc(FlowWorkList1, SSAWorkList1, Environment4)
      end
  end;
%% Take an element from the SSAWorkList and process it
scc([], [{Node, Instruction} | SSAWorkList], Environment) ->
  case reachable(Node, Environment) of
    true ->
      case ?CODE:is_phi(Instruction) of
        true ->
          {Environment1, MoreSSA} = visit_phi(Instruction, Node, Environment),
          scc([], MoreSSA ++ SSAWorkList, Environment1);
        false ->
          {MoreFlowDests, MoreSSAWork, Environment1} = 
            visit_expression(Instruction, Environment),
          SSAWorkList1 = MoreSSAWork ++ SSAWorkList,
          MoreFlowWork = [{Node, Destination} || Destination<-MoreFlowDests],
          scc(MoreFlowWork, SSAWorkList1, Environment1)
      end;
    false ->
      scc([], SSAWorkList, Environment)
  end.

%%-----------------------------------------------------------------------------
%% Procedure : update_cfg/1
%% Purpose   : Transforms the cfg into something more pleasant.
%%             Here the mapping of variables & registers to lattice-values is 
%%             used to actually change the code.
%% Arguments : Environment - in which everything happens.
%% Returns   : A new CFG.
%%-----------------------------------------------------------------------------

update_cfg(Environment) ->
  NodeList = get_nodelist(Environment),
  CFG1     = update_nodes(NodeList, Environment),
  %% why not hipe_???_ssa:remove_dead_code ?
  CFG2     = ?CFG:remove_unreachable_code(CFG1),
  CFG2.

%%-----------------------------------------------------------------------------
%% Procedure : update_nodes/2
%% Purpose   : loop over all nodes in a list of nodes, ignoring any 
%%             non-reachable node.
%% Arguments : NodeList - the list of nodes.
%%             Environment - in which everything happens.
%% Returns   : a new cfg.
%%-----------------------------------------------------------------------------

update_nodes([], Environment) ->
  env__cfg(Environment);
update_nodes([Node | NodeList], Environment) ->
  NewEnvironment =
    case reachable(Node, Environment) of
      true ->
	Instructions = extract_code(Node, Environment),
	Updater = fun(Instruction) ->
		      update_instruction(Instruction, Environment)
		  end,
	NewInstructions = lists:flatmap(Updater, Instructions),
	update_code(Node, NewInstructions, Environment);
      false -> 
	Environment
    end,
  update_nodes(NodeList, NewEnvironment).

%%-----------------------------------------------------------------------------
%% Procedure : update_code/3
%% Purpose   : Insert a list of new instructions into the cfg in the
%%             environment
%% Arguments : Node - name of the bb whose instructions we replace.
%%             NewInstructions - The list of new instructions
%%             Env  - The environment
%% Returns   : A new environment
%%-----------------------------------------------------------------------------

update_code(Node, NewInstructions, Environment) ->
  CFG                 = env__cfg(Environment),
  BB                  = ?CFG:bb(CFG, Node),
  OrderedInstructions = put_phi_nodes_first(NewInstructions),
  NewBB               = hipe_bb:code_update(BB, OrderedInstructions),
  NewCFG              = ?CFG:bb_add(CFG, Node, NewBB),
  Environment#env{cfg = NewCFG}.

%%-----------------------------------------------------------------------------
%% Procedure : put_phi_nodes_first/1
%% Purpose   : Move all phi-instructions to the beginning of the basic block.
%% Arguments : Instructions - The list of instructions
%% Returns   : A list of instructions where the phi-nodes are first.
%%-----------------------------------------------------------------------------

put_phi_nodes_first(Instructions) ->
  {PhiInstructions, OtherInstructions} = 
    partition(fun(X) -> ?CODE:is_phi(X) end, Instructions),
  PhiInstructions ++ OtherInstructions.

%%-----------------------------------------------------------------------------

partition(Function, List) ->
  partition(Function, List, [], []).

partition(_Function, [], True, False) ->
  {lists:reverse(True), lists:reverse(False)};

partition(Function, [Hd | Tail], True, False) ->
  case Function(Hd) of
    true ->
      partition(Function, Tail, [Hd | True], False);
    false ->
      partition(Function, Tail, True, [Hd | False])
  end.

%%-----------------------------------------------------------------------------
%% Procedure : visit_phi_nodes/4
%% Purpose   : visit all the phi-nodes in a bb and return the list of 
%%             remaining instructions, new ssa-edges and a new environment.
%% Arguments : [Inst|Insts] - The list of instructions in the bb
%%             Node - Name of the current node.
%%             Environment - the environment
%%             SSAWork  - the ssawork found so far.
%% Returns   : {Environment, Instruction list, SSAWorkList}
%%-----------------------------------------------------------------------------

visit_phi_nodes([], CurrentNode, _Environment, _SSAWork) ->
  ?EXIT({"~w: visit_phi_nodes/4 Basic block contains no code",
	 ?MODULE, CurrentNode});
visit_phi_nodes(Is = [Inst | Insts], Node, Environment, SSAWork) ->
  case ?CODE:is_phi(Inst) of
    true ->
      {Environment1, NewSSA} = visit_phi(Inst, Node, Environment),
      visit_phi_nodes(Insts, Node, Environment1, NewSSA ++ SSAWork);
    false ->
      {Environment, Is, SSAWork}
  end.

%%-----------------------------------------------------------------------------
%% Procedure : visit_phi/3
%% Purpose   : visit a phi-node
%% Arguments : PhiInstruction- The instruction
%%             CurrentNode - Name of the current node.
%%             Environment - the environment
%% Returns   : {NewEnvironment, SSAWork}
%%-----------------------------------------------------------------------------

visit_phi(PhiInstruction, CurrentNode, Environment) ->
  ArgumentList = ?CODE:phi_arglist(PhiInstruction),
  Value        = get_phi_value(ArgumentList, CurrentNode, Environment, top),
  Name         = ?CODE:phi_dst(PhiInstruction),
  {Environment1, SSAWork} = update_lattice_value({Name, Value}, Environment),
  {Environment1, SSAWork}.

%%-----------------------------------------------------------------------------
%% Procedure : get_phi_value/4
%% Purpose   : compute the result of a phi-function, taking care to ignore 
%%             edges that are not yet executable.
%% Arguments : ArgList - the list of arguments {Node, Value pair}
%%             CurrentNode - the current node
%%             Environment - well...
%%             CurrentValue - the meet of the relevant already processed values
%% Returns   : Integer, top or bottom
%%-----------------------------------------------------------------------------

%% the arglist contains {predecessor, variable} elements.  Remember
%% to be optimistic in this part, hopefully, topvalues will fall down
%% to become constants. Hence topvalues are more or less ignored here.
get_phi_value([], _CurrentNode, _Environment, CurrentValue) ->
  CurrentValue;
get_phi_value([{PredecessorNode, Variable}| ArgList],
              CurrentNode,
              Environment,
              CurrentValue) ->
  case executable({PredecessorNode, CurrentNode}, Environment) of
    true ->
      NewValue = lookup_lattice_value(Variable, Environment),
      case NewValue of
        bottom ->
          bottom;
        top ->
          get_phi_value(ArgList, CurrentNode, Environment, CurrentValue);
        _ ->
          case CurrentValue of
            top -> 
              get_phi_value(ArgList, CurrentNode, Environment, NewValue);
            _ ->
              case (NewValue =:= CurrentValue) of
                true ->
                  get_phi_value(ArgList, CurrentNode, Environment, NewValue);
                false ->  %% two different constants.
                  bottom
              end
          end
      end;
    false ->  %% non-executable transitions don't affect the value.
      get_phi_value(ArgList, CurrentNode, Environment, CurrentValue)
  end.

%%------------------------------ environment ----------------------------------

reachable(Node, Environment) ->
  Predecessors = predecessors(Node, Environment),
  Executable   = fun(Pred) -> executable({Pred, Node}, Environment) end,
  lists:any(Executable, Predecessors).

%%-----------------------------------------------------------------------------

mark_as_executable(Edge, Environment) ->
  ExecutableFlags  = env__executable_flags(Environment),
  ExecutableFlags1 = gb_sets:add(Edge, ExecutableFlags),
  Environment#env{executable_flags = ExecutableFlags1}.

%%-----------------------------------------------------------------------------

mark_as_handled(Node, Environment = #env{handled_blocks=Handled}) ->
  NewHandled = gb_sets:add_element(Node, Handled),
  Environment#env{handled_blocks=NewHandled}.

handled(Node, #env{handled_blocks=Handled}) ->
  gb_sets:is_element(Node, Handled).

%%-----------------------------------------------------------------------------

extract_code(Node, Environment) ->
  CFG = env__cfg(Environment),
  case ?CFG:bb(CFG, Node) of
    not_found -> ?WARNING_MSG("Could not find label ~w.\n", [Node]),
                 [];
    BB -> hipe_bb:code(BB)
  end.

%%-----------------------------------------------------------------------------

predecessors(Node, Environment) ->
  CFG = env__cfg(Environment),
  ?CFG:pred(CFG, Node).

%%-----------------------------------------------------------------------------

executable(Edge, Environment) ->
  ExecutableFlags = env__executable_flags(Environment),
  gb_sets:is_member(Edge, ExecutableFlags).

%%-----------------------------------------------------------------------------

update_lattice_value({[], _NewValue}, Environment) ->
  {Environment, []};
update_lattice_value({Names, NewValue}, Environment) when is_list(Names) ->
  Update = 
    fun(Dst, {Env, SSA}) ->
        {NewEnv, NewSSA} = 
          update_lattice_value({Dst, NewValue}, Env),
        {NewEnv, SSA ++ NewSSA}
    end,
  lists:foldl(Update, {Environment, []}, Names);
%% update_lattice_value({Name, {Res, N, Z, C, V} }, _) ->
%%   ?EXIT({"inserting dumt grejs", {Name, {Res, N, Z, C, V} } });
update_lattice_value({Name, NewValue}, Environment) ->
  LatticeValues = env__lattice_values(Environment),
  {LatticeValues1, SSAWork} = 
    case gb_trees:lookup(Name, LatticeValues) of
      none ->
        {gb_trees:insert(Name, NewValue, LatticeValues),
	 lookup_ssa_edges(Name, Environment)};
      {value, NewValue} ->
	{LatticeValues, []};
      {value, _} ->
        {gb_trees:update(Name, NewValue, LatticeValues),
	 lookup_ssa_edges(Name, Environment)}
    end,
  {Environment#env{lattice_values = LatticeValues1}, SSAWork}.

%%-----------------------------------------------------------------------------

lookup_ssa_edges(Variable, Environment) ->
  SSAEdges = env__ssa_edges(Environment),
  case gb_trees:lookup(Variable, SSAEdges) of
    {value, X} ->
      X;
    _ -> % Unused variable
      []
  end.

%%-----------------------------------------------------------------------------

get_nodelist(Environment) ->
  CFG = env__cfg(Environment),
  ?CFG:labels(CFG).

%%-----------------------------------------------------------------------------

-ifdef(DEBUG).

%%-----------------------------------------------------------------------------
%%---------------------------------- DEBUG ------------------------------------

error(Text) ->
  error(Text, []).

error(Text, Data) ->
  io:format("Internal compiler error in ~w\n",[?MODULE]),
  io:format(Text, Data),
  io:format("\n\n"),
  halt().

%%-----------------------------------------------------------------------------

print_environment(Environment) ->
  io:format("============================================================\n"),
  io:format("Executable flags: "),
  print_executable_flags(env__executable_flags(Environment)),
  io:format("Lattice values --->\n"),
  print_lattice_values(env__lattice_values(Environment)),
  io:format("SSA edges --->\n"),
  print_ssa_edges(env__ssa_edges(Environment)),
  io:format("============================================================\n").
  
%%-----------------------------------------------------------------------------

print_executable_flags(ExecutableFlags) ->
  ListOfFlags = gb_sets:to_list(ExecutableFlags),
  Printer     = fun ({Source, Destination}) -> 
		    io:format("(~w, ~w), ", [Source, Destination]) end,
  lists:foreach(Printer, ListOfFlags),
  io:format("()\n").

%%-----------------------------------------------------------------------------

print_lattice_values(LatticeValues) ->
  ListOfLatticeValues = gb_trees:to_list(LatticeValues),
  Printer             = fun ({Key, Value}) ->
			    io:format("~w = ~w\n", [Key, Value]) end,
  lists:foreach(Printer, ListOfLatticeValues).

%%-----------------------------------------------------------------------------

print_ssa_edges(SSAEdges) ->
  ListOfSSAEdges = gb_trees:to_list(SSAEdges),
  Printer        = fun ({Key, Value}) ->
		       io:format("~w: ~w\n", [Key, Value]) end,
  lists:foreach(Printer, ListOfSSAEdges).

%%-----------------------------------------------------------------------------

-endif.	%% DEBUG

%%-----------------------------------------------------------------------------

