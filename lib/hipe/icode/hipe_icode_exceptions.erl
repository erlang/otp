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
%% ====================================================================
%%  Filename : 	hipe_icode_exceptions.erl
%%  Module   :	hipe_icode_exceptions
%%  Purpose  :  Rewrite calls in intermediate code to use Continuation
%%              and Fail-To labels.
%%
%%		Catch-instructions work as follows:
%%                - A begin_try(FailLabel) starts a catch-region which
%%                  is ended by a corresponding end_try(FailLabel).
%%                - The handler begins with a begin_handler(FailLabel).
%%
%%		However, the begin/end instructions do not always appear
%%		as parentheses around the section that they protect (in
%%		linear Beam/Icode). Also, different begin_catch
%%		instructions can reach the same basic blocks (which may
%%		raise exceptions), due to code compation optimizations
%%		in the Beam compiler, even though they have different
%%		handlers. Because of this, a data flow analysis is
%%		necessary to find out which catches may reach which
%%		basic blocks. After that, we clone basic blocks as
%%		needed to ensure that each block belongs to at most one
%%		unique begin_catch. The Beam does not have this problem,
%%		since it will find the correct catch-handler frame
%%		pushed on the stack. (Note that since there can be no
%%		tail-calls within a catch region, our dataflow analysis
%%		for finding all catch-stacks is sure to terminate.)
%%
%%		Finally, we can remove all special catch instructions
%%		and rewrite calls within catch regions to use explicit
%%		fail-to labels, which is the main point of all this.
%%		Fail labels that were set before this pass are kept.
%%		(Note that calls that have only a continuation label do
%%		not always end their basic blocks. Adding a fail label
%%		to such a call can thus force us to split the block.)
%%
%%  Notes    :  As of November 2003, primops that do not fail in the 
%%              normal sense are allowed to have a fail-label even
%%              before this pass. (Used for the mbox-empty + get_msg
%%              primitive in receives.)
%%
%%              Native floating point operations cannot fail in the
%%              normal sense. Instead they throw a hardware exception
%%              which will be caught by a special fp check error
%%              instruction. These primops do not need a fail label even
%%              in a catch. This pass checks for this with
%%              hipe_icode_primops:fails/1. If a call cannot fail, no
%%              fail label is added.
%%
%%              Explicit fails (exit, error and throw) inside
%%              a catch have to be handled. They have to build their
%%              exit value and jump directly to the catch handler. An
%%              alternative solution would be to have a new type of
%%              fail instruction that takes a fail-to label...
%% ====================================================================

-module(hipe_icode_exceptions).

-export([fix_catches/1]).

-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").

%%----------------------------------------------------------------------------

-spec fix_catches(cfg()) -> cfg().

fix_catches(CFG) ->
  {Map, State} = build_mapping(find_catches(init_state(CFG))),
  hipe_icode_cfg:remove_unreachable_code(get_cfg(rewrite(State, Map))).

%% This finds the set of possible catch-stacks for each basic block

find_catches(State) ->
  find_catches(get_start_labels(State),
	       clear_visited(clear_changed(State))).

find_catches([L|Ls], State0) ->
  case is_visited(L, State0) of
    true ->
      find_catches(Ls, State0);
    false ->
      State1 = set_visited(L, State0),
      Code = get_bb_code(L, State1),
      Cs = get_new_catches_in(L, State1),
      State2 = set_catches_in(L, Cs, State1),  % memorize
      Cs1 = catches_out(Code, Cs),
      Ls1 = get_succ(L, State2) ++ Ls,
      Cs0 = get_catches_out(L, State2),
      if Cs1 =:= Cs0 ->
	  find_catches(Ls1, State2);
	 true ->
	  State3 = set_catches_out(L, Cs1, State2),
	  find_catches(Ls1, set_changed(State3))
      end
  end;
find_catches([], State) ->
  case is_changed(State) of
    true ->
      find_catches(State);
    false ->
      State
  end.

catches_out([I|Is], Cs) ->
  catches_out(Is, catches_out_instr(I, Cs));
catches_out([], Cs) ->
  Cs.

catches_out_instr(I, Cs) ->
  case I of
    #icode_begin_try{} ->
      Id = hipe_icode:begin_try_label(I),
      push_catch(Id, Cs);
    #icode_end_try{} ->
      pop_catch(Cs);
    #icode_begin_handler{} ->
      pop_catch(Cs);
    _ ->
      Cs
  end.


%% This builds the mapping used for cloning

build_mapping(State) ->
  build_mapping(get_start_labels(State), clear_visited(State),
		new_mapping()).

build_mapping([L|Ls], State0, Map) ->
  case is_visited(L, State0) of
    true ->
      build_mapping(Ls, State0, Map);
    false ->
      State1 = set_visited(L, State0),
      Cs = list_of_catches(get_catches_in(L, State1)),  % get memorized
      {Map1, State2} = map_bb(L, Cs, State1, Map),
      Ls1 = get_succ(L, State2) ++ Ls,
      build_mapping(Ls1, State2, Map1)
  end;
build_mapping([], State, Map) ->
  {Map, State}.

map_bb(_L, [_C], State, Map) ->
  {Map, State};
map_bb(L, [C | Cs], State, Map) ->
  %% This block will be cloned - we need to create N-1 new labels.
  %% The identity mapping will be used for the first element.
  Map1 = new_catch_labels(Cs, L, Map),
  State1 = set_catches_in(L, single_catch(C), State),  % update catches in
  Code = get_bb_code(L, State1),
  State2 = clone(Cs, L, Code, State1, Map1),
  {Map1, State2}.

clone([C | Cs], L, Code, State, Map) ->
  Ren = get_renaming(C, Map),
  L1 = Ren(L),
  State1 = set_bb_code(L1, Code, State),
  State2 = set_catches_in(L1, single_catch(C), State1),  % set catches in
  clone(Cs, L, Code, State2, Map);
clone([], _L, _Code, State, _Map) ->
  State.

new_catch_labels([C | Cs], L, Map) ->
  L1 = hipe_icode:label_name(hipe_icode:mk_new_label()),
  Map1 = set_mapping(C, L, L1, Map),
  new_catch_labels(Cs, L, Map1);
new_catch_labels([], _L, Map) ->
  Map.


%% This does all the actual rewriting and cloning.

rewrite(State, Map) ->
  rewrite(get_start_labels(State), clear_visited(State), Map).

rewrite([L|Ls], State0, Map) ->
  case is_visited(L, State0) of
    true ->
      rewrite(Ls, State0, Map);
    false ->
      State1 = set_visited(L, State0),
      Code = get_bb_code(L, State1),
      Cs = list_of_catches(get_catches_in(L, State1)),  % get memorized
      State2 = rewrite_bb(L, Cs, Code, State1, Map),
      Ls1 = get_succ(L, State2) ++ Ls,
      rewrite(Ls1, State2, Map)
  end;
rewrite([], State, _Map) ->
  State.

rewrite_bb(L, [C], Code, State, Map) ->
  {Code1, State1} = rewrite_code(Code, C, State, Map),
  set_bb_code(L, Code1, State1).

rewrite_code(Is, C, State, Map) ->
  rewrite_code(Is, C, State, Map, []).

rewrite_code([I|Is], C, State, Map, As) ->
  [C1] = list_of_catches(catches_out_instr(I, single_catch(C))),
  case I of
    #icode_begin_try{} ->
      {I1, Is1, State1} = update_begin_try(I, Is, C, State, Map),
      I2 = redirect_instr(I1, C, Map),
      rewrite_code(Is1, C1, State1, Map, [I2 | As]);
    #icode_end_try{} ->
      rewrite_code(Is, C1, State, Map, As);
    #icode_call{} ->
      {I1, Is1, State1} = update_call(I, Is, C, State, Map),
      I2 = redirect_instr(I1, C, Map),
      rewrite_code(Is1, C1, State1, Map, [I2 | As]);
    #icode_fail{} ->
      {I1, Is1, State1} = update_fail(I, Is, C, State, Map),
      I2 = redirect_instr(I1, C, Map),
      rewrite_code(Is1, C1, State1, Map, [I2 | As]);
    _ ->
      I1 = redirect_instr(I, C, Map),
      rewrite_code(Is, C1, State, Map, [I1 | As])
  end;
rewrite_code([], _C, State, _Map, As) ->
  {lists:reverse(As), State}.

redirect_instr(I, C, Map) ->
  redirect_instr_1(I, hipe_icode:successors(I), get_renaming(C, Map)).

redirect_instr_1(I, [L0 | Ls], Ren) ->
  I1 = hipe_icode:redirect_jmp(I, L0, Ren(L0)),
  redirect_instr_1(I1, Ls, Ren);
redirect_instr_1(I, [], _Ren) ->
  I.

update_begin_try(I, Is, _C, State0, _Map) ->
  L = hipe_icode:begin_try_successor(I),
  I1 = hipe_icode:mk_goto(L),
  {I1, Is, State0}.

update_call(I, Is, C, State0, Map) ->
  case top_of_stack(C) of
    [] ->
      %% No active catch. Assume cont./fail labels are correct as is.
      {I, Is, State0};
    L ->
      %% Only update the fail label if the call *can* fail.
      case hipe_icode_primops:fails(hipe_icode:call_fun(I)) of
	true ->
	  %% We only update the fail label if it is not already set.
	  case hipe_icode:call_fail_label(I) of
	    [] ->
	      I1 = hipe_icode:call_set_fail_label(I, L),
	      %% Now the call will end the block, so we must put the rest of
	      %% the code (if nonempty) in a new block!
	      if Is =:= [] ->
		  {I1, Is, State0};
		 true ->
		  L1 = hipe_icode:label_name(hipe_icode:mk_new_label()),
		  I2 = hipe_icode:call_set_continuation(I1, L1),
		  State1 = set_bb_code(L1, Is, State0),
		  State2 = set_catches_in(L1, single_catch(C), State1),
		  State3 = rewrite_bb(L1, [C], Is, State2, Map),
		  {I2, [], State3}
	      end;
	    _ when Is =:= [] ->
	      %% Something is very wrong if Is is not empty here. A call
	      %% with a fail label should have ended its basic block.
	      {I, Is, State0}
	  end;
	false ->
	  %% Make sure that the fail label is not set.
	  I1 = hipe_icode:call_set_fail_label(I, []),
	  {I1, Is, State0}
      end
  end.

update_fail(I, Is, C, State, _Map) ->
  case hipe_icode:fail_label(I) of
    [] -> 
      {hipe_icode:fail_set_label(I, top_of_stack(C)), Is, State};
    _ ->
      {I, Is, State}
  end.


%%---------------------------------------------------------------------
%% Abstraction for sets of catch stacks.

%% This is the bottom element
no_catches() -> [].

%% A singleton set
single_catch(C) -> [C].

%% A single, empty stack
empty_stack() -> [].

%% Getting the label to fail to
top_of_stack([C|_]) -> C;
top_of_stack([]) -> [].    % nil is used in Icode for "no label"

join_catches(Cs1, Cs2) ->
  ordsets:union(Cs1, Cs2).

list_of_catches(Cs) -> Cs.

%% Note that prepending an element to all elements in the list will
%% preserve the ordering of the list, and will never make two existing
%% elements become identical, so the list is still an ordset. 

push_catch(L, []) ->
  [[L]];
push_catch(L, Cs) ->
  push_catch_1(L, Cs).

push_catch_1(L, [C|Cs]) ->
  [[L|C] | push_catch_1(L, Cs)];
push_catch_1(_L, []) ->
  [].

%% However, after discarding the head of all elements, the list
%% is no longer an ordset, and must be processed.

pop_catch(Cs) ->
  ordsets:from_list(pop_catch_1(Cs)).

pop_catch_1([[_|C] | Cs]) ->
  [C | pop_catch_1(Cs)];
pop_catch_1([[] | Cs]) ->
  %% The elements in the list represent different possible incoming
  %% stacks of catch handlers to this BB.  Before the fixpoint has
  %% been found these elements are underapproximations of the true
  %% stacks, therefore it's possible for these elements to be too
  %% short for the number of pops implied by the code in the BB.
  %% We must not fail in that case, so we set pop([]) = [].
  %% This fixes find_catches_crash.erl and compiler_tests in the
  %% HiPE test suite.
  [[] | pop_catch_1(Cs)];
pop_catch_1([]) ->
  [].


%%---------------------------------------------------------------------
%% Mapping from catch-stacks to renamings on labels.

new_mapping() ->
  gb_trees:empty().

set_mapping(C, L0, L1, Map) ->
  Dict = case gb_trees:lookup(C, Map) of
	   {value, Dict0} ->
	     gb_trees:enter(L0, L1, Dict0);
	   none ->
	     gb_trees:insert(L0, L1, gb_trees:empty())
	 end,
  gb_trees:enter(C, Dict, Map).

%% Return a label renaming function for a particular catch-stack

get_renaming(C, Map) ->
  case gb_trees:lookup(C, Map) of
    {value, Dict} ->
      fun (L0) ->
	  case gb_trees:lookup(L0, Dict) of
	    {value, L1} -> L1;
	    none -> L0
	  end
      end;
    none ->
      fun (L0) -> L0 end
  end.


%%---------------------------------------------------------------------
%% State abstraction

-record(state, {cfg					:: cfg(),
		changed = false				:: boolean(),
		succ					:: cfg(),
		pred					:: cfg(),
		start_labels				:: [icode_lbl(),...],
		visited = hipe_icode_cfg:none_visited()	:: gb_sets:set(),
		out     = gb_trees:empty()		:: gb_trees:tree(),
		in      = gb_trees:empty()		:: gb_trees:tree()
	       }).

init_state(CFG) ->
  SLs = [hipe_icode_cfg:start_label(CFG)],
  #state{cfg = CFG, succ = CFG, pred = CFG, start_labels = SLs}.

get_cfg(State) ->
  State#state.cfg.

get_start_labels(State) ->
  State#state.start_labels.

get_pred(L, State) ->
  hipe_icode_cfg:pred(State#state.pred, L).

get_succ(L, State) ->
  hipe_icode_cfg:succ(State#state.succ, L).

set_changed(State) ->
  State#state{changed = true}.

is_changed(State) ->
  State#state.changed.

clear_changed(State) ->
  State#state{changed = false}.

set_catches_out(L, Cs, State) ->
  State#state{out = gb_trees:enter(L, Cs, State#state.out)}.

get_catches_out(L, State) ->
  case gb_trees:lookup(L, State#state.out) of
    {value, Cs} -> Cs;
    none -> no_catches()
  end.

set_catches_in(L, Cs, State) ->
  State#state{in = gb_trees:enter(L, Cs, State#state.in)}.

get_catches_in(L, State) ->
  case gb_trees:lookup(L, State#state.in) of
    {value, Cs} -> Cs;
    none -> no_catches()
  end.

set_visited(L, State) ->
  State#state{visited = hipe_icode_cfg:visit(L, State#state.visited)}.

is_visited(L, State) ->
  hipe_icode_cfg:is_visited(L, State#state.visited).

clear_visited(State) ->
  State#state{visited = hipe_icode_cfg:none_visited()}.

get_bb_code(L, State) ->
  hipe_bb:code(hipe_icode_cfg:bb(State#state.cfg, L)).

set_bb_code(L, Code, State) ->
  CFG = State#state.cfg,
  CFG1 = hipe_icode_cfg:bb_add(CFG, L, hipe_bb:mk_bb(Code)),
  SLs = [hipe_icode_cfg:start_label(CFG1)],
  State#state{cfg = CFG1, succ = CFG1, pred = CFG1, start_labels = SLs}.

get_new_catches_in(L, State) ->
  Ps = get_pred(L, State),
  Cs = case lists:member(L, get_start_labels(State)) of
	 true -> single_catch(empty_stack());
	 false -> no_catches()
       end,
  get_new_catches_in(Ps, Cs, State).

get_new_catches_in([P | Ps], Cs, State) ->
  Cs1 = join_catches(Cs, get_catches_out(P, State)),
  get_new_catches_in(Ps, Cs1, State);
get_new_catches_in([], Cs, _) ->
  Cs.

%%---------------------------------------------------------------------
